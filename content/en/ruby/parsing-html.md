---
title:                "Parsing html"
html_title:           "Ruby recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means extracting specific data or information from an HTML document. This is commonly done by programmers to automate data extraction from websites, such as scraping product information for an e-commerce website or retrieving data for analytics purposes.

## How to:

To parse HTML in Ruby, we can use the Nokogiri gem. First, we need to install the gem by running `gem install nokogiri` in the terminal. Then, we can use the gem in our code by requiring it at the top: `require 'nokogiri'`. Here's an example of how to parse an HTML document and get the title:

```Ruby
# require nokogiri
require 'nokogiri'

# create Nokogiri object from html document
doc = Nokogiri::HTML(html_document)

# get title tag from document
title = doc.css("title").text

# print title
puts title
```

Output: "My Website"

## Deep Dive

Parsing HTML has been a common practice among programmers since the early days of web scraping. Before the introduction of gems like Nokogiri, developers had to manually write complex code to extract data from HTML documents. With Nokogiri, the process has become much simpler and more efficient.

An alternative to Nokogiri is using regular expressions to parse HTML, but this can be more complex and prone to errors. Nokogiri, on the other hand, uses a powerful and user-friendly API to navigate and extract data from HTML documents.

Nokogiri internally uses the libxml2 and libxslt libraries, which are written in C, to perform the parsing and data extraction process. This makes Nokogiri faster and more efficient compared to other methods.

## See Also

- [Nokogiri gem](https://github.com/sparklemotion/nokogiri)
- [Libxml2](http://xmlsoft.org/libxml2/) and [libxslt](http://xmlsoft.org/libxslt/) libraries used by Nokogiri