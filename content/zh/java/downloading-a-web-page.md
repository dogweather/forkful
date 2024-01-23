---
title:                "下载网页"
date:                  2024-01-20T17:44:33.854371-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/downloading-a-web-page.md"
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
