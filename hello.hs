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

-- Embedded Domain Specific Language
-- Domain Specific Language: specialized language for specific domain such as CSS or HTML
-- Embedded DSL: DSL language embedded in another programming language that produces a valid DSL language
-- Combinator Pattern
-- Define primitives and combinators
-- Primitives - basic building blocks of language
-- Combinators - functions that combine the primitives

-- <> is string concatenation operator
-- <> has right fixity. Ex "a" <> "b" <> "c" === "a" <> ("b" <> "c")
-- -> is right associative. ex: String -> String -> String === String -> (String -> String)
-- -- three = (\num1 -> \num2 -> num1 + num2) 1 2
-- -- -> marks start of function body
-- three :: Int -> Int -> Int
-- three = (\num1 num2 -> num1 + num2)
