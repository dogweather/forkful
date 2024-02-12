---
title:                "Завантаження веб-сторінки"
aliases: - /uk/cpp/downloading-a-web-page.md
date:                  2024-01-20T17:43:51.744974-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
