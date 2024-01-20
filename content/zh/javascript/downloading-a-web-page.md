---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 下载网页——是什么和为什么?

下载网页是指使用程序将网页的源代码抓取并保存到本地的过程。程序员之所以要做这个，主要是为了离线查看、数据分析等需求。

# 如何做：

以下是一个用JavaScript下载网页的简单示例。当然，子函数`downloadPage`需要另一个库`node-fetch`来运行。

```Javascript
const fetch = require('node-fetch');
const fs = require('fs');

async function downloadPage(url) {
    const response = await fetch(url);
    const text = await response.text();

    fs.writeFileSync("web-page.html", text);
}

downloadPage("https://www.example.com");
```
上述代码会把 `example.com` 主页的HTML内容保存到本地的一个名为 `web-page.html` 的文件里。

# 深入了解：

下载网页并不是一个新颖的概念，它的历史几乎与互联网的历史一样长。在JavaScript中，我们有多种下载网页的方式，比如使用内建的`http`模块，或者使用像`axios`和`node-fetch`这样的第三方库。

每种方法都有各自的优势，例如node-fetch提供了基于Promise的API，这使得异步操作更加直观和方便。

实际上，下载网页只是获取信息的第一步。之后，你可能需要对下载到的内容进行解析——比如，抽取出其中的链接、文本或者图片。幸运的是，JavaScript提供了一些优秀的库来帮助你处理这个问题，比如 `cheerio`。

# 参考资料：

1. [node-fetch Documentation](https://github.com/node-fetch/node-fetch)
2. [Axios Documentation](https://axios-http.com/docs/intro)
3. [Cheerio Documentation](https://cheerio.js.org/)
4. Understanding of Web Scraping[Web Scraping Overview](https://en.wikipedia.org/wiki/Web_scraping)