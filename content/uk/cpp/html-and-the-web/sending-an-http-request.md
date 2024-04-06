---
date: 2024-01-20 17:59:17.929815-07:00
description: "How to: / \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: Sending an HTTP request in C++ usually involves leveraging libraries because\
  \ the standard C++ library doesn't provide this\u2026"
lastmod: '2024-04-05T22:51:02.778648-06:00'
model: gpt-4-1106-preview
summary: "/ \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\
  \ Sending an HTTP request in C++ usually involves leveraging libraries because the\
  \ standard C++ library doesn't provide this functionality directly. Historically,\
  \ programmers often used libcurl, but C++ now has the `cpprestsdk` (formerly known\
  \ as Casablanca) which simplifies these tasks. Alternatives to `cpprestsdk` include\
  \ Boost.Beast or POCO libraries. When implementing an HTTP request, consider the\
  \ type (GET, POST, etc.), error handling, resource cleanup, and platform-specific\
  \ behavior."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## How to: / Як це зробити:
```C++
#include <iostream>
#include <cpprest/http_client.h>
#include <cpprest/filestream.h>

int main() {
    auto fileStream = std::make_shared<concurrency::streams::ostream>();

    // Відкрийте потік для запису у файл
    concurrency::streams::fstream::open_ostream(U("result.html")).then(
        [fileStream](concurrency::streams::ostream outFile) {
            *fileStream = outFile;

            // Створіть HTTP-клієнта
            web::http::client::http_client client(U("http://example.com"));

            // Відправте запит GET
            return client.request(web::http::methods::GET);
        }
    ).then([fileStream](web::http::http_response response) {
        // Направте відповідь у потік файлу
        return response.body().read_to_end(fileStream->streambuf());
    }).then([fileStream](size_t) {
        return fileStream->close();
    }).wait();

    std::cout << "HTTP request sent. Check 'result.html' for response." << std::endl;

    return 0;
}
```

Sample output:
```
HTTP request sent. Check 'result.html' for response.
```

## Deep Dive / Заглиблення:
Sending an HTTP request in C++ usually involves leveraging libraries because the standard C++ library doesn't provide this functionality directly. Historically, programmers often used libcurl, but C++ now has the `cpprestsdk` (formerly known as Casablanca) which simplifies these tasks. Alternatives to `cpprestsdk` include Boost.Beast or POCO libraries. When implementing an HTTP request, consider the type (GET, POST, etc.), error handling, resource cleanup, and platform-specific behavior.

## See Also / Дивіться також:
- [cpprestsdk GitHub](https://github.com/Microsoft/cpprestsdk)
- [libcurl](https://curl.se/libcurl/)
- [Boost.Beast](https://www.boost.org/doc/libs/release/libs/beast/)
- [POCO Libraries](https://pocoproject.org/)
