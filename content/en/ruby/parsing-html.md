---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML involves taking a HTML document as an input and breaking it down into readable elements for manipulation purposes. Programmers do it to extract data, automate web tasks, or modify web content.

## How to:

Parsing HTML in Ruby is a pretty straightforward process. Nokogiri is the most popular HTML parsing library for Ruby.

First, install Nokogiri:

```Ruby
gem install nokogiri
```

To parse HTML:

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open('http://www.example.com'))
puts doc
```

This will print the parsed HTML of the example webpage. To extract specific elements, use CSS selectors:

```Ruby
doc.css('h1').each do |header|
  puts header.content
end
```

This code extracts all `<h1>` tags from the parsed HTML and prints their content.

## Deep Dive:

In the early days of the web, HTML tags were regex-matched, which was cumbersome and error-prone. Nokogiri, introduced in 2007, significantly simplified this process. While other libraries like Hpricot have become obsolete or discontinued, Nokogiri, being quick, reliable, and easy to use, remains popular in Ruby applications.

There are alternatives to Nokogiri, such as Oga and rexml. These can be faster but lack Nokogiri's extensive feature set. 

Nokogiri parses HTML with a native C extension that leverages the libxml2 library. This makes it fast, but also means it might be slower to install than pure Ruby alternatives, as it needs to compile a C extension during installation.

## See Also:

- [Nokogiri documentation](https://nokogiri.org/)
- [libxml2 library](http://xmlsoft.org/)
- [Oga libray](https://github.com/YorickPeterse/oga)
- [rexml library](https://github.com/ruby/rexml)