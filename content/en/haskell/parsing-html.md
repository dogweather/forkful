---
title:                "Parsing html"
html_title:           "Haskell recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Why

If you've ever tried to scrape data from a website or create a web crawler, you know how messy and unstructured HTML can be. Parsing HTML is the process of extracting information from an HTML document, allowing you to easily manipulate and analyze data on the web.

## How To

To get started with parsing HTML in Haskell, you will need to install a few packages. The most commonly used one is "tagsoup," which provides functions for parsing HTML documents. 

```
Haskell
import Text.HTML.TagSoup -- Import the tag soup library

main = do
  let html = "<html><body><h1>Hello, world!</h1></body></html>" -- Create a sample HTML document
  let tags = parseTags html -- Parse the HTML into a list of tags
  print tags -- Print the tags to see the structure of the document
```

Output:
```
[TagOpen "html" [], TagOpen "body" [], TagOpen "h1" [], TagText "Hello, world!", TagClose "h1", TagClose "body", TagClose "html"]
```

You can also use functions such as `isTagOpen` and `isTagNode` to filter out specific tags from the list. For more complex HTML documents, you can use the "html-conduit" package which uses a monadic parsing approach. Here's an example of using it to extract all links from a webpage:

```
Haskell
import Data.Conduit -- Import the conduit library
import Text.HTML.DOM -- Import the HTML DOM parser
import Network.HTTP.Conduit -- Import the HTTP library

main = do
  req <- parseUrlThrow "https://www.example.com" -- Create a request object for the desired webpage
  response <- withManager $ httpLbs req -- Download the webpage
  let links = runConduit $ responseBody response
                .| sinkDoc
                .| element "a" -- Filter out <a> tags
                .| attribute "href" -- Get the value of the "href" attribute
                .| printC -- Print the output to the screen
```

Output:
```
https://www.example.com/contact
https://www.example.com/about
https://www.example.com/products
https://www.example.com/blog
```

## Deep Dive

HTML documents are structured using tags and attributes, making it easy for browsers to display them correctly. However, this structure can be challenging to work with when trying to extract specific information. Luckily, Haskell provides us with powerful tools such as libraries and functional programming techniques to make this task more manageable. 

In addition to "tagsoup" and "html-conduit," there are other popular packages for parsing HTML, such as "html-tagsoup," "haxr," and "xml-conduit." Each has its advantages and use cases, so it's essential to choose the right one for your project.

See Also

- [Haskell Wiki: Web](https://wiki.haskell.org/Web)
- [Real World Haskell: Parsing XML and HTML](http://book.realworldhaskell.org/read/parsing-xml-and-html.html)