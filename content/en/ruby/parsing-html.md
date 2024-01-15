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

## Why

Have you ever wanted to extract specific information from a website? Whether it's for data analysis, web scraping, or automation, parsing HTML can help you easily retrieve the data you need. With Ruby, parsing HTML is simple and efficient.

## How To

```Ruby
# First, install the 'nokogiri' gem for parsing HTML
gem install nokogiri

# Next, require the gem in your Ruby file
require 'nokogiri'

# Create a new Nokogiri document by parsing the HTML webpage
doc = Nokogiri::HTML(open("https://www.example.com"))

# Now you can use CSS or XPath selectors to locate elements in the HTML document
# For example, to retrieve the title of the webpage:
title = doc.at_css("title").text
# Output: "Example Domain"

# You can also retrieve multiple elements using CSS selectors:
links = doc.css("a")
# Output: Returns an array of all anchor tags on the webpage

# To extract specific information, you can use regular expressions
email = doc.at("a[href*='mailto:']").text
# Output: Returns the email address listed on the webpage

# Finally, you can store the data you extract in variables or manipulate it as needed
```

## Deep Dive

Parsing HTML essentially involves extracting data from a structured markup language. With Nokogiri, you can use CSS or XPath selectors to access specific elements in the HTML document. Regular expressions can also be used to retrieve text that matches a certain pattern. It's worth noting that HTML parsing can be affected by the structure and organization of the webpage, so it's important to understand the DOM (Document Object Model) before diving into parsing.

## See Also

- [Nokogiri documentation](https://nokogiri.org/)
- [CSS selectors](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPath tutorial](https://www.w3schools.com/xml/xpath_intro.asp)