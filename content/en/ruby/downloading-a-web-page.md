---
title:                "Downloading a web page"
html_title:           "Ruby recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you are working on a project that involves scraping data from the web, downloading a webpage can be a crucial step. Whether you are a data scientist, web developer, or just a curious individual, downloading a webpage can provide you with valuable information and insights.

## How To

To download a webpage using Ruby, you can utilize the "net/http" library. First, you need to require the library in your code:

```Ruby
require 'net/http'
```

Then, you can use the "Net::HTTP.get" method to download the webpage and store it in a variable:

```Ruby
page = Net::HTTP.get(URI("https://www.example.com"))
```

You can also specify the user agent and other headers to mimic a browser request:

```Ruby
url = URI.parse("https://www.example.com")
request = Net::HTTP::Get.new(url.to_s, {'User-Agent' => 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) Gecko/20100101 Firefox/91.0'})
response = Net::HTTP.start(url.host, url.port, use_ssl: url.scheme == 'https') do |http|
  http.request(request)
end

page = response.body
```

Once you have downloaded the webpage, you can do further processing, such as parsing HTML, extracting specific data, or saving the page locally.

## Deep Dive

When downloading a webpage with Ruby, it is important to consider the different ways it can be accessed. For example, if a website requires authentication, you may need to use a separate library or method to handle it. Additionally, you may encounter errors such as timeouts or invalid SSL certificates, which you need to handle in your code.

Another aspect to consider is the performance of your code. If you are downloading multiple webpages, you may want to consider using parallelization techniques to speed up the process. You can also implement caching to avoid multiple downloads of the same webpage.

See Also

- The official Ruby documentation for "net/http": https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html
- A tutorial on web scraping with Ruby: https://www.freecodecamp.org/news/web-scraping-with-ruby-tutorial/
- A blog post on optimizing web scraping with Ruby: https://blog.arkency.com/7-tips-to-enhance-your-web-scraping-scripts-with-ruby/