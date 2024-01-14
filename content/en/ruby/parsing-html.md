---
title:                "Ruby recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML, also known as web scraping, is a useful skill for any programmer. It allows you to extract data from web pages and use it for various purposes. This can include data analysis, automation, or creating web crawlers.

## How To

First, we need to install a Ruby gem called Nokogiri, which is specifically designed to parse HTML. Open your terminal and run the following command:

```Ruby
gem install nokogiri
```

Next, let's create a new Ruby file and require the Nokogiri gem:

```Ruby
require 'nokogiri'
```

We can use Nokogiri to parse HTML from a given URL or a local HTML file. Let's start by parsing the HTML from a website. For this example, we will use the Nokogiri website. We will also use the `open-uri` library to handle the HTTP request.

```Ruby
require 'nokogiri'
require 'open-uri'

url = "https://nokogiri.org/"
document = Nokogiri::HTML(open(url))
```

Now, we can use Nokogiri's methods to navigate and extract data from the HTML. For example, to get the title of the webpage, we can use the `title` method:

```Ruby
puts document.title

# Output: Nokogiri -- Home
```

We can also use CSS selectors with Nokogiri to extract specific elements from the page. For instance, to get all the links from the page, we can use the `css` method to target the `<a>` tags:

```Ruby
links = document.css('a')

links.each do |link|
  puts link.content
end

# Output:
# Documentation
# Tutorial
# Download
# GitHub
# Blog
# Forum
# Community
```

## Deep Dive

Nokogiri is powerful and has many other features that can make parsing HTML a breeze. It supports XPath expressions, which allows you to target specific elements using path-like syntax. It also has built-in methods for scraping data from tables, forms, and RSS feeds.

Nokogiri also has error handling capabilities and can handle malformed HTML, making it a reliable tool for parsing any web page.

## See Also

- [Nokogiri official documentation](https://nokogiri.org/tutorials)
- [Ruby OpenURI documentation](http://ruby-doc.org/stdlib-2.4.0/libdoc/open-uri/rdoc/OpenURI.html)
- [XPath tutorial](https://www.w3schools.com/xml/xpath_intro.asp)