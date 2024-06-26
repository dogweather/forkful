---
date: 2024-01-20 15:33:38.456428-07:00
description: "How to: To parse HTML in Ruby, install the 'Nokogiri' gem with `gem\
  \ install nokogiri`. Nokogiri is like a Swiss Army knife for working with HTML and\
  \ XML\u2026"
lastmod: '2024-03-13T22:45:00.548790-06:00'
model: unknown
summary: To parse HTML in Ruby, install the 'Nokogiri' gem with `gem install nokogiri`.
title: Parsing HTML
weight: 43
---

## How to:
To parse HTML in Ruby, install the 'Nokogiri' gem with `gem install nokogiri`. Nokogiri is like a Swiss Army knife for working with HTML and XML in Ruby. Here's a quick example:

```ruby
require 'nokogiri'
require 'open-uri'

# Load HTML content from a website
html_content = URI.open('http://example.com').read

# Parse the HTML
doc = Nokogiri::HTML(html_content)

# Extract the title
title = doc.xpath('//title').text
puts "The title of the page is: #{title}"
```

This spits out something like: `The title of the page is: Example Domain`.

## Deep Dive
Back in the early Ruby days, options for parsing HTML were limited. REXML was built-in but slow. Then Hpricot showed up, but it fizzled out. Nokogiri debuted in 2008, blending the ease of Hpricot with the speed and power of libxml, a proven XML toolkit.

In the parsing world, there are always alternatives. Some swear by the built-in 'rexml' library or 'oga', another XML/HTML parser for Ruby. But Nokogiri remains a favorite for its robustness and speed, not to mention its vast array of features.

Under the hood, Nokogiri converts HTML into a Document Object Model (DOM)—a tree structure. This makes it easy to navigate and manipulate elements. Using XPath and CSS selectors, you can pinpoint any piece of information you need.

## See Also
- Nokogiri gem: [https://nokogiri.org/](https://nokogiri.org/)
- Ruby's rexml documentation: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternative parser 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Learn about XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
