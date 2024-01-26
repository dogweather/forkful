---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:17.929815-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request.md"
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
