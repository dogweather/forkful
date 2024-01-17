---
title:                "Parsing html"
html_title:           "C# recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# An Informal Guide to Parsing HTML Using C#

## What & Why?
So you want to know about parsing HTML, huh? Well, put simply, parsing HTML is the process of taking raw HTML code and converting it into a more structured format that can be read and manipulated by a computer. Programmers do this in order to extract specific information from websites or to automate repetitive tasks.

## How to:
The most popular way to parse HTML using C# is by using the HtmlAgilityPack library. Here's a simple example of how to use it to extract all the links from a webpage:

```C#
// Import the HtmlAgilityPack library
using HtmlAgilityPack;

// Create a new HtmlDocument object and load the webpage
HtmlDocument document = new HtmlDocument();
document.Load("https://www.example.com");

// Use LINQ to select all <a> tags and extract their "href" attributes
var links = document.DocumentNode.Descendants("a")
                .Select(a => a.GetAttributeValue("href", null))
                .Where(link => !String.IsNullOrEmpty(link));
```

And that's it! You now have all the links from the webpage stored in the `links` variable. Of course, parsing HTML can involve much more complex tasks, but this gives you a basic idea of how it works.

## Deep Dive:
Parsing HTML has been around since the early days of the internet when information was primarily shared through websites. Before the introduction of libraries like HtmlAgilityPack, programmers had to manually write code to extract information from HTML, a tedious and time-consuming process.

Nowadays, there are several alternatives to HtmlAgilityPack, such as AngleSharp and CsQuery. Each library has its own strengths and weaknesses, so it's important to do your research and choose the one that best fits your project's needs.

When it comes to implementation details, parsing HTML can be a bit tricky. HTML code is not always well-formed, meaning it may not follow the proper syntax rules. In these cases, libraries like HtmlAgilityPack use heuristics and guessing algorithms to make sense of the code and extract the desired information.

## See Also:
- Official website of HtmlAgilityPack: https://html-agility-pack.net/
- AngleSharp library: https://anglesharp.github.io/
- CsQuery library: https://github.com/jamietre/CsQuery