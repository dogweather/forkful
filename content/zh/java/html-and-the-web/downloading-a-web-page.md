---
date: 2024-01-20 17:44:33.854371-07:00
description: "How to: (\u5982\u4F55\u6267\u884C\uFF1A) \u5728\u4E92\u8054\u7F51\u65E9\
  \u671F\uFF0C\u7F51\u9875\u4E0B\u8F7D\u4E3B\u8981\u4E3A\u5B58\u6863\u548C\u6162\u901F\
  \u8FDE\u63A5\u73AF\u5883\u4E0B\u7684\u79BB\u7EBF\u6D4F\u89C8\u3002\u5982\u4ECA\uFF0C\
  \u9664\u4E86\u4E0A\u8FF0 `java.io` \u548C `java.net` \u5305\uFF0C\u4E5F\u53EF\u7528\
  \u7B2C\u4E09\u65B9\u5E93\u5982 Apache's HttpClient \u6216 Jsoup\u3002\u7B2C\u4E09\
  \u65B9\u5E93\u901A\u5E38\u63D0\u4F9B\u66F4\u591A\u529F\u80FD\uFF0C\u6BD4\u5982\u89E3\
  \u6790 HTML\uFF0C\u5904\u7406 cookies\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.831305-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u6267\u884C\uFF1A) \u5728\u4E92\u8054\u7F51\u65E9\u671F\uFF0C\
  \u7F51\u9875\u4E0B\u8F7D\u4E3B\u8981\u4E3A\u5B58\u6863\u548C\u6162\u901F\u8FDE\u63A5\
  \u73AF\u5883\u4E0B\u7684\u79BB\u7EBF\u6D4F\u89C8\u3002\u5982\u4ECA\uFF0C\u9664\u4E86\
  \u4E0A\u8FF0 `java.io` \u548C `java.net` \u5305\uFF0C\u4E5F\u53EF\u7528\u7B2C\u4E09\
  \u65B9\u5E93\u5982 Apache's HttpClient \u6216 Jsoup\u3002\u7B2C\u4E09\u65B9\u5E93\
  \u901A\u5E38\u63D0\u4F9B\u66F4\u591A\u529F\u80FD\uFF0C\u6BD4\u5982\u89E3\u6790 HTML\uFF0C\
  \u5904\u7406 cookies \u548C\u66F4\u590D\u6742\u7684 HTTP \u8BF7\u6C42\u3002\u5B9E\
  \u73B0\u65F6\u8FD8\u8981\u6CE8\u610F\u6CD5\u5F8B\u548C\u9053\u5FB7\u95EE\u9898\uFF0C\
  \u6BD4\u5982\u9075\u5B88 `robots.txt` \u548C\u4E0D\u6EE5\u7528\u7F51\u7AD9\u8D44\
  \u6E90\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
