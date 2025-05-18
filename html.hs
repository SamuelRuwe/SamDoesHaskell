main :: IO()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title"
    ( append_ 
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )

newtype Html = Html String
newtype Structure = Structure String
type Title = String

render :: Html -> String
render html =
  case html of
    Html str -> str

html_ :: Title -> Structure -> Html
html_ title content = Html
  ( el "html"
    ( el "head" (el "title" title)
      <> el "body" (getStructureString content)
    )
  )

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
