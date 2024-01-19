---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/parsing-html.md"
---

{{< edit_this_page >}}

---

## 什么与为什么？
解析HTML意味着我们将一个HTML文件转换为可供编程处理的结构。我们这样做以便在web页面抓取数据，或改变或查看页面内容。

## 如何操作：

让我们使用Nokogiri gem来解析HTML。首先，我们需要安装这个gem。运行下面的命令来安装：

```Ruby
gem install nokogiri
```

然后，我们可以使用下面的代码例子来解析HTML：

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open('http://www.example.com'))

doc.xpath('//h1').each do |node|
  puts node.text
end
```

在这个例子中，我们从www.example.com获取HTML，并寻找所有的`<h1>`标签。`node.text`就是`<h1>`标签中的文本。

## 深入学习：

解析HTML的概念可以追溯到www的早期年代，当时网页主要由HTML构成，数据抓取及分析变得可能并且重要。有多种方式可以进行HTML解析，例如使用正则表达式，直接字符串处理，或如这里演示的Nokogiri。Nokogiri的实现细节非常复杂，它在底层使用C和Java语言进行HTML和XML的解析。

## 了解更多：

如果你对HTML解析有更多的兴趣，可以看看下面的资源：

1. Nokogiri 官方文档: http://nokogiri.org/
2. HTML 解析教程: https://www.tutorialspoint.com/ruby-on-rails/rails-html-parsing.htm
3. 正则表达式教程: https://www.rubyguides.com/2015/06/ruby-regex/

---

注:请注意网络的安全性与数据的敏密性问题，在实践抓取时遵守相关法规条款，尊重网站的Robot协议。