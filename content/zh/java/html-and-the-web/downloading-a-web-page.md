---
date: 2024-01-20 17:44:33.854371-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u662F\u6307\u5C06\u7F51\u4E0A\u7684\u9875\u9762\
  \u4FDD\u5B58\u5230\u672C\u5730\u3002\u7F16\u7A0B\u5E08\u8FD9\u4E48\u505A\u4EE5\u6536\
  \u96C6\u6570\u636E\u3001\u6D4B\u8BD5\u7F51\u7AD9\u6216\u8FDB\u884C\u5185\u5BB9\u5907\
  \u4EFD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.624118-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u662F\u6307\u5C06\u7F51\u4E0A\u7684\u9875\u9762\
  \u4FDD\u5B58\u5230\u672C\u5730\u3002\u7F16\u7A0B\u5E08\u8FD9\u4E48\u505A\u4EE5\u6536\
  \u96C6\u6570\u636E\u3001\u6D4B\u8BD5\u7F51\u7AD9\u6216\u8FDB\u884C\u5185\u5BB9\u5907\
  \u4EFD\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
下载网页是指将网上的页面保存到本地。编程师这么做以收集数据、测试网站或进行内容备份。

## How to: (如何执行：)
```java
import java.io.*;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String webUrl = "http://www.example.com";
        try (BufferedInputStream in = new BufferedInputStream(new URL(webUrl).openStream());
             FileOutputStream fileOutputStream = new FileOutputStream("downloaded_page.html")) {
            byte dataBuffer[] = new byte[1024];
            int bytesRead;
            while ((bytesRead = in.read(dataBuffer, 0, 1024)) != -1) {
                fileOutputStream.write(dataBuffer, 0, bytesRead);
            }
            System.out.println("下载完成。文件保存在 downloaded_page.html.");
        } catch (IOException e) {
            System.out.println("发生错误：" + e.getMessage());
        }
    }
}
```
输出样例：
```
下载完成。文件保存在 downloaded_page.html.
```

## Deep Dive (深入了解)
在互联网早期，网页下载主要为存档和慢速连接环境下的离线浏览。如今，除了上述 `java.io` 和 `java.net` 包，也可用第三方库如 Apache's HttpClient 或 Jsoup。第三方库通常提供更多功能，比如解析 HTML，处理 cookies 和更复杂的 HTTP 请求。实现时还要注意法律和道德问题，比如遵守 `robots.txt` 和不滥用网站资源。

## See Also (另请参阅)
- Oracle 官方文档的 InputStream 类: https://docs.oracle.com/javase/8/docs/api/java/io/InputStream.html
- Apache HttpClient: http://hc.apache.org/httpcomponents-client-ga/
- Jsoup: https://jsoup.org/
