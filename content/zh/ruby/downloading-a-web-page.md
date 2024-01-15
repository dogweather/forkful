---
title:                "下载网页"
html_title:           "Ruby: 下载网页"
simple_title:         "下载网页"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

下网页是一项常见的任务，它可以让我们轻松地获取网页上的信息。例如，我们可以使用Ruby来下载一个新闻网站的文章，以便我们可以在离线状态下阅读它们。

## 如何

使用Ruby下载网页非常简单。首先，我们需要安装一个名为"open-uri"的gem（gem是Ruby的库）。在安装完成后，我们需要使用这个gem提供的`open`方法来打开一个URL。

```Ruby
require 'open-uri'

url = "https://www.example.com/"
html = open(url).read
```

上面的代码将打开`https://www.example.com/`这个网址，并把网页的内容读取到`html`变量中。

我们也可以使用`open`方法来下载文件，例如图片：

```Ruby
require 'open-uri'

url = "https://www.example.com/example_image.png"
open(url) do |image|
  File.open("example_image.png", 'wb') do |file|
    file.write(image.read)
  end
end
```

上面的代码将从指定的URL下载一张图片，并保存到本地文件夹中。

## 深入探讨

`open-uri`其实是一个很强大的工具，它不仅可以下载网页，还可以发送HTTP请求，处理cookie等功能。它的文档提供了更多关于如何使用它的详细信息。此外，我们也可以使用其他的Ruby库，如`httparty`或`net/http`来完成下载网页的任务。深入了解这些库的功能，将帮助我们更好地处理网络请求。

## 参考链接

- open-uri文档：https://ruby-doc.org/stdlib-2.5.1/libdoc/open-uri/rdoc/OpenURI.html
- HTTParty文档：https://github.com/jnunemaker/httparty
- net/http文档：https://ruby-doc.org/stdlib-2.5.1/libdoc/net/http/rdoc/Net/HTTP.html