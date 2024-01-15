---
title:                "下载网页。"
html_title:           "Fish Shell: 下载网页。"
simple_title:         "下载网页。"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

在当今的互联网时代，我们每天都会浏览大量的网页，并可能需要将其中某些内容保存下来。使用Fish Shell可以轻松地下载网页，并将网页内容保存到本地。

## 如何进行

```Fish Shell``` 有一个内置的功能 ```curl``` 可以帮助我们下载网页。我们可以使用下面的命令来下载一个网页，并将其保存为一个文件：

```
curl https://www.example.com > example.html
```

如果我们想要在下载的同时显示下载进度，我们可以使用 ```--progress-bar``` 参数：

```
curl --progress-bar https://www.example.com > example.html
```

如果我们想要将下载的网页文件保存到指定的文件夹，我们可以使用 ```-o``` 参数：

```
curl -o ~/Downloads/example.html https://www.example.com
```

## 深入了解

除了上面提到的参数外，```curl``` 还有很多其他有用的选项，可以帮助我们更加灵活地下载网页。例如，我们可以通过 ```-L``` 参数来自动重定向链接，```-k``` 参数来忽略证书验证等。

可以通过以下命令来查看所有可用的选项：

```
man curl
```

## 参考链接

- Fish Shell官方网站：https://fishshell.com/
- ```curl``` 的官方文档：https://curl.se/docs/
- ```man``` 的官方文档：https://man7.org/linux/man-pages/