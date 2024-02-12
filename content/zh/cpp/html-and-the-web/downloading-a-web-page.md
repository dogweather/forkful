---
title:                "下载网页"
aliases: - /zh/cpp/downloading-a-web-page.md
date:                  2024-01-20T17:43:56.852156-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
下载网页，就是从网上把网页内容拿到本地。程序员这么做是为了分析数据，监控网站状态，或是自动化测试。

## How to: 如何操作
```C++
#include <iostream>
#include <curl/curl.h>

size_t callback(void *contents, size_t size, size_t nmemb, std::string *s) {
  size_t newLength = size * nmemb;
  try {
    s->append((char*)contents, newLength);
  } catch(std::bad_alloc &e) {
    // Handle memory problem
    return 0;
  }
  return newLength;
}

int main() {
  CURL *curl;
  CURLcode res;
  std::string response;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    res = curl_easy_perform(curl);
    if(CURLE_OK == res) {
      std::cout << response << '\n';
    } else {
      std::cerr << "CURL Error: " << curl_easy_strerror(res) << std::endl;
    }
    
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
### 输出样例：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive 深入挖掘
下载网页不是什么新鲜事，在90年代早期web出现之初就已经有了。这个功能最初是通过命令行工具实现的，比如`wget`或`curl`。后来，随着编程语言的发展，库如`libcurl`允许我们在代码中直接实施这些操作。

这里示范的`libcurl`，是一个非常流行的跨平台的库支持多种协议，比如HTTP、HTTPS和FTP等。它也提供了易于使用的API以及良好的文档支持。

其他选择包括 `Poco` 和 `Boost.Asio` （两者支持HTTP客户端功能）。截止到2023年，`Boost.Beast` 这个库也是受欢迎的选择，它依赖于 `Boost.Asio`，且专注于网络。

实现时，注意一定要处理好内存和异常。示例中的错误处理非常基础，实际项目中可能需要更复杂的逻辑来处理网络异常和数据解析问题。

## See Also 参考链接
- cURL 官方网站: [https://curl.se/](https://curl.se/)
- libcurl 文档: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- Boost.Asio 库文档: [https://www.boost.org/doc/libs/release/libs/asio/](https://www.boost.org/doc/libs/release/libs/asio/)
- Poco Libraries: [https://pocoproject.org/](https://pocoproject.org/)

记得，在真正使用这些库的时候，要去看看具体的文档和教程，确保用得地道。
