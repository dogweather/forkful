---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Завантаження веб-сторінки - це процес отримання даних, що представляють сторінку, з сервера. Ми, програмісти, робимо це, щоб обробляти та аналізувати зміст веб-сторінок автоматично.

## Як це зробити:

Ось приклад коду на С, що завантажує веб-сторінку і виводить вміст у консоль:

```C
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://www.example.com";
    char outfilename[FILENAME_MAX] = "page.html";
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl); 
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

## Поглиблено:

Завантаження веб-сторінок існує практично стільки, скільки і сам Інтернет. Із часом появились альтернативні методи, як HTTP/2, HTTPS, та інші. Вищенаведений код використовує бібліотеку libcurl для обробки HTTP запитів. Вона подолає багато нюансів, що виникають під час ручного написання HTTP клієнта.

## Дивіться також:

Для більш детальної інформації про libcurl дивіться офіційну документацію: https://curl.haxx.se/libcurl/c/

Можете прочитати і про інші бібліотеки для завантаження веб-сторінок:
- POCO C++ Libraries: https://pocoproject.org/docs/Poco.Net.HTTPClientSession.html
- Boost Asio (для C++): https://think-async.com/Asio/
- WinINet (Windows API): https://docs.microsoft.com/en-us/windows/win32/wininet/about-wininet