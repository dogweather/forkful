---
date: 2024-01-20 17:43:51.744974-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u0414\u043B\
  \u044F \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438 \u0432 C++\
  \ \u043C\u043E\u0436\u0435\u043C\u043E \u0441\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u0442\u0438\u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u043E\u044E cURL. \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\u0430\u0434\
  ."
lastmod: '2024-03-13T22:44:49.841219-06:00'
model: gpt-4-1106-preview
summary: "\u0414\u043B\u044F \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\
  \u043D\u043D\u044F \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\
  \u0438 \u0432 C++ \u043C\u043E\u0436\u0435\u043C\u043E \u0441\u043A\u043E\u0440\u0438\
  \u0441\u0442\u0430\u0442\u0438\u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u043E\u044E cURL."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

## Як зробити:
Для завантаження веб-сторінки в C++ можемо скористатися бібліотекою cURL. Ось приклад:

```C++
#include <iostream>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res) {
            std::cout << "Page content:" << std::endl << readBuffer << std::endl;
        } else {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return 0;
}
```
При запуску коду видасть вміст сторінки http://example.com.

## Поглиблено:
Завантаження веб-сторінок – давня ідея, що стартувала з народженням інтернету. Історично, це робилося через CLI засоби типу `wget` чи `curl`. У C++, `libcurl` є однією з основних бібліотек для таких завдань, проте існують альтернативи, як `Boost.Asio` чи різні HTTP клієнтські бібліотеки. При використанні `libcurl`, важливими деталями є обробка повернених callbacks та правильне управління пам'яттю.

## Дивись також:
- Документація libcurl: https://curl.se/libcurl/
- Інструкція по роботі з бібліотекою Boost.Asio: https://www.boost.org/doc/libs/release/libs/asio/
- Репозиторій GitHub з HTTP клієнтськими бібліотеки для C++: https://github.com/yhirose/cpp-httplib
