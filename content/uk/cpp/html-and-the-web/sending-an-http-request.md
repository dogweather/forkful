---
date: 2024-01-20 17:59:17.929815-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP \u0437\u0430\
  \u043F\u0438\u0442\u0443 - \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\u0431 \u043E\
  \u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438 \u043C\u0456\
  \u0436 \u0432\u0430\u0448\u0438\u043C \u0434\u043E\u0434\u0430\u0442\u043A\u043E\
  \u043C C++ \u0442\u0430 \u0441\u0435\u0440\u0432\u0435\u0440\u043E\u043C \u0432\
  \ \u0456\u043D\u0442\u0435\u0440\u043D\u0435\u0442\u0456. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435 \u0434\u043B\u044F \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\
  \u044F, \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ \u0447\u0438\u2026"
lastmod: '2024-03-11T00:14:23.653337-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP \u0437\u0430\
  \u043F\u0438\u0442\u0443 - \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\u0431 \u043E\
  \u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438 \u043C\u0456\
  \u0436 \u0432\u0430\u0448\u0438\u043C \u0434\u043E\u0434\u0430\u0442\u043A\u043E\
  \u043C C++ \u0442\u0430 \u0441\u0435\u0440\u0432\u0435\u0440\u043E\u043C \u0432\
  \ \u0456\u043D\u0442\u0435\u0440\u043D\u0435\u0442\u0456. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435 \u0434\u043B\u044F \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\
  \u044F, \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ \u0447\u0438\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Відправка HTTP запиту - це спосіб обміну даними між вашим додатком C++ та сервером в інтернеті. Програмісти роблять це для отримання, відправлення чи зміни інформації віддалено.

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
