---
title:                "C# recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Why

If you have ever worked with websites or web applications, chances are you have come across HTML code. This markup language is the basis for all web pages and is used to structure content and create layouts. However, as a programmer, you may encounter situations where you need to extract information from HTML code, either for data analysis or to automate certain tasks. This is where parsing HTML comes in handy.

## How To

Parsing HTML essentially means extracting specific data from a web page and converting it into a structured format that can be easily manipulated by code. In C#, there are several tools and libraries available to help with this task. One popular option is the HtmlAgilityPack, which provides a high-level API for working with HTML data.

Let's take a look at a simple example. Suppose we have the following HTML code:

```C#
<html>
<body>
<div class="user">
<h2>John Doe</h2>
<p>Age: 30</p>
</div>
</body>
</html>
```

We can use the HtmlAgilityPack library to extract the name and age of the user and display it in our console. Here's the code to achieve this:

```C#
var html = new HtmlDocument();
html.LoadHtml(htmlCode);

//select the div with the class name "user"
var userDiv = html.DocumentNode.SelectSingleNode("//div[@class='user']");

//get the name and age values from the h2 and p tags
var name = userDiv.SelectSingleNode("//h2").InnerText;
var age = userDiv.SelectSingleNode("//p").InnerText;

//print out the results
Console.WriteLine($"Name: {name}");
Console.WriteLine($"Age: {age}");
```

The output of this code would be:

```
Name: John Doe
Age: Age: 30
```

Of course, this is a simplified example, but it showcases the basics of parsing HTML with the HtmlAgilityPack library. You can also use other tools such as LINQ to XML or regular expressions for parsing HTML, but the HtmlAgilityPack provides a more efficient and convenient way to work with HTML data.

## Deep Dive

Before diving into parsing HTML, it's important to understand the structure of HTML code. HTML uses tags enclosed in angle brackets to define elements such as headings, paragraphs, tables, etc. These tags can also have attributes that provide additional information about the element.

In order to parse HTML effectively, you need to have a solid understanding of these tags and their corresponding attributes. Using tools like the HtmlAgilityPack, you can select specific elements or attributes using XPath syntax, which is based on XML. This allows you to extract the desired data from the HTML code.

Another important aspect to consider when parsing HTML is handling errors and exceptions. Since HTML code can vary greatly, it's essential to have robust error handling to prevent your code from breaking when encountering unexpected data.

## See Also

- [HtmlAgilityPack Documentation](http://html-agility-pack.net/documentation)
- [Introduction to HTML](https://www.w3schools.com/html/html_intro.asp)
- [XPath Syntax Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)

Parsing HTML may seem daunting at first, but with the right tools and knowledge, it can be a powerful tool in your programming arsenal. Happy coding!