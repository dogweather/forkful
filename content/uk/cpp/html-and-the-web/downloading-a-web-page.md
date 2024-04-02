---
date: 2024-01-20 17:43:51.744974-07:00
description: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438 \u2013 \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0457\u0457 \u0432\u043C\u0456\u0441\u0442\u0443 \u0447\u0435\
  \u0440\u0435\u0437 \u0406\u043D\u0442\u0435\u0440\u043D\u0435\u0442. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\
  \u0443 \u0434\u0430\u043D\u0438\u0445, \u043C\u043E\u043D\u0456\u0442\u043E\u0440\
  \u0438\u043D\u0433\u0443 \u0432\u043C\u0456\u0441\u0442\u0443 \u0447\u0438 \u0430\
  \u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0437\u0430\u0446\u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:49.841219-06:00'
model: gpt-4-1106-preview
summary: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438 \u2013 \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0457\u0457 \u0432\u043C\u0456\u0441\u0442\u0443 \u0447\u0435\
  \u0440\u0435\u0437 \u0406\u043D\u0442\u0435\u0440\u043D\u0435\u0442. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\u0437\
  \u0443 \u0434\u0430\u043D\u0438\u0445, \u043C\u043E\u043D\u0456\u0442\u043E\u0440\
  \u0438\u043D\u0433\u0443 \u0432\u043C\u0456\u0441\u0442\u0443 \u0447\u0438 \u0430\
  \u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0437\u0430\u0446\u0456\u0457\u2026"
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

## Що та Навіщо?
Завантаження веб-сторінки – це процес отримання її вмісту через Інтернет. Програмісти роблять це для аналізу даних, моніторингу вмісту чи автоматизації веб-інтерактивів.

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
