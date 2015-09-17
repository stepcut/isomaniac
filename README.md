ISOMANIAC
=========

This is a prototype of a Haskell-based framework for building user
interfaces in the web browser. It is influenced by the
Model-Update-View pattern, Virtual DOMs, and isomorphic frameworks.

MURV
----

At the core of any isomaniac application is a `MURV` value:

```haskell
data MURV  model action remote = MURV
    { model  :: model
    , update :: action -> model -> (model, Maybe remote)
    , view   :: model  -> HTML action
    }
```

We can see three key components, `model`, `update`, and `view`. The
`model` is a pure value containing all the data relevant to the
current application state. `view` is a pure function which renders the
HTML for the `model`. Because `view` is pure, it will always generate
the same HTML for the same `model`. This, in theory, allows the HTML
to be created on the serverside or clientside. Though, isomaniac does
not yet exploit that option.

The final component of `MURV` is the `update` function:

    update :: action -> model -> (model, Maybe remote)

`update` takes an `action`, the current `model` and returns a new
`model` and an optional `remote` action. The `update` function is also
pure.

All the impurity is wrapped up in the `murv` function:

```haskell
murv :: (ToJSString remote, Show action) =>
        Text                     -- ^ remote API URL
     -> (Text -> action)         -- ^ convert a remote response to an 'action'
     -> MURV model action remote -- ^ model-update-remote-view record
     -> (Maybe action)           -- ^ initial action
     -> IO ()
```

Let's back up and look at a simple counter example. The aim is to create a webpage with the follow features:

 1. a count value
 2. a paragraph that displays the current count
 3. a text field which can be used to change the count
 4. an increment button
 5. a decrement button

The first thing we will do is create a data type for the `model`:

```haskell
data Model = Model
    { count :: Int
    , msg   :: Text
    }
```

Our model has two fields -- the current `count` and a `msg` field that
can be used to display various messages to the user. The `msg` field
might be used if they enter a string that is not a number or to
display server communication errors.

Next we need a type to represent the various `actions` that can affect the `model`:

```haskell
data Action
    = Increment
    | Decrement
    | Msg Text
    | Set (Maybe JSString)
```

The `Increment` and `Decrement` actions increment or decrement the
current count. The `Msg Text` action sets the message text. `Set
(Maybe JSString)` attempts to set the count using the current value in
the text input field.

The `update'` function looks at the `action` and updates the `model`
accordingly. For the `remote` action we are using simple string types
in this example. A more robust example would use a shared data type
for communication.

```haskell
{- Update -}
update' :: Action -> Model -> (Model, Maybe Text)
update' action model =
    case action of
      Increment -> (model { count = (count model) + 1 }, Just "inc")
      Decrement -> (model { count = (count model) - 1 }, Just "dec")
      Msg txt   -> (model { msg = txt }, Nothing)
      Set (Just jstr)  ->
          case reads (fromJSString jstr) :: [(Int, String)] of
            [(n, _)] -> (model { count = n }, Just "set")
            _ -> (model, Nothing)
      Set _ -> (model, Nothing)
```
The other function we need is the `view'` function:

```haskell
view' :: Model -> HTML Action
view' (Model c txt) =
    [hsx| <div>
            <p>The count is <% show c %></p>
            <button onclick=Decrement>-</button>
            <button onclick=Increment>+</button>
            <input type="text" oninput=Set value=(pack $ show c) />
            <p><% txt %></p>
          </div>
        |]
```

The `hsx` quasiquoter permits the use of XML syntax in the view
function. We see that the actions are simply used as values for the
`onclick` and `oninput` attributes.

We then bundle these pieces up into a `MURV` value:

```haskell
counter :: MURV Model Action Text
counter = MURV
  { model  = Model 0 "Nothing to Say."
  , update = update'
  , view   = view'
  }
```
To evaluate the `counter` app we use the `murv` function:

```haskell
main :: IO ()
main = murv "http://localhost:8000/api" Msg counter Nothing
```

The URL is the remote API. The value returned from remote calls is
turned into an action by appling the `Msg` constructor.

Our server is very simple -- performing two tasks. One is to serve the
app from the `Main.jsexe` directory. The other is to simply echo back
any requests it gets from the client.

Virtual DOM
-----------

In this example we re-render the entire HTML everytime the model is
updated. However, updating the real DOM on every change can be very
slow and also cause issues with the focus being lost.

Behind the scenes, isomaniac works by patching the DOM rather than
recreating it. It diffs the old and new `HTML` and creates patches
which are applied to the browser DOM.

TODO
----

At present, isomaniac lacks composability. If we had a second widget
with its own model, update, and view functions, there is no obvious
way to combine them. The solution to this is likely to involve lenses.

Another issue solidyfing the interface between the client and
server. Using a simple algrebraic type is a good start, but as the
application grows, it becomes clear that something fancier is
desired. When the client sends a request it has expectations on what
subset of values should be returned.

The current Diff/Patch code mostly works, but definitely has at least
one bug. A more robust implementation would be good. Additionally,
obsolete event listeners need to be cleaned up. Additionally, the
event listeners should probably be implemented differently
anyway. Also, there are issues with the cursor position and updates to
the DOM.

Another aim of isomaniac is to ensure that something desirable happens
if the user reloads the page. It's also desireable to be able to
render the page content even if javascript is not enabled for the sake
of search engines. The pure model used by isomaniac and the fact that
the view function can be run on the server or client makes it seem
reasonable that they features can be implemented with out too much
difficulty.
