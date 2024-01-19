---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么和为什么？

下载网页就是从服务器获取网页的内容，并将其保存在本地。程序员常常需要下载网页，以便分析网页内容信息、数据挖掘、测试网页响应等。

## 具体怎么做

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class DownloadWebPage {
  public static void main(String[] argv) throws Exception {
    URL url = new URL("http://www.example.com");
    URLConnection connection = url.openConnection();
    BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
    String inputLine;
    while ((inputLine = in.readLine()) != null) 
      System.out.println(inputLine);
    in.close();
  }
}
```
以上 Java 代码可以下载 `http://www.example.com` 页面的 HTML 内容，运行输出为下载的 HTML 内容。

## 深度了解

### 历史背景
网页下载是网络爬虫技术的基础，源于互联网初期对信息自动获取的需求。

### 可选方案 
Java 除去直接使用 `URLConnection`，还可以利用各种第三方库如 `Jsoup`，这是一个更高级、更为便利的工具。

### 实现细节
上述 Java 程序通过 `URL` 和 `URLConnection` 对象访问网络，读取并打印网页的内容。要读取更多信息，如头信息，可以使用 `URLConnection` 相关方法。

## 请参阅

[Jsoup 官方文档](https://jsoup.org/)

[Oracle 官方网络教程](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)