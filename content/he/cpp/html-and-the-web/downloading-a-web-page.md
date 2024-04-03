---
date: 2024-01-20 17:43:50.418816-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D1-C++, \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA \u05D1\u05E9\
  \u05DD `cURL` \u05DC\u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.834053-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D1-C++, \u05E0\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\
  \u05D9\u05EA \u05D1\u05E9\u05DD `cURL` \u05DC\u05D3\u05D5\u05D2\u05DE\u05D4."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## איך לעשות:
כדי להוריד דף אינטרנט ב-C++, נשתמש בספריית פופולרית בשם `cURL` לדוגמה.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

size_t callbackfunction(void *ptr, size_t size, size_t nmemb, std::string *data) {
    data->append((char*) ptr, size * nmemb);
    return size * nmemb;
}

std::string download_html(const std::string &url) {
    CURL *curl;
    CURLcode res;
    std::string response_data;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callbackfunction);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return response_data;
}

int main() {
    std::string url = "http://example.com";
    std::string html_content = download_html(url);
    
    if (!html_content.empty()) {
        std::cout << "HTML content downloaded successfully:" << std::endl;
        std::cout << html_content << std::endl;
    } else {
        std::cout << "Failed to download the content." << std::endl;
    }

    return 0;
}
```
זה ידפיס את ה-HTML של http://example.com.

## עיון מעמיק:
הורדת דף אינטרנט לא הייתה פשוטה תמיד. לפני cURL, היינו צריכים להשתמש ב-sockets ולדבר ישירות עם ברוטוקול HTTP. חלופות ל-cURL כוללות ספריות כמו `Boost.Beast` ו`Poco`. אם אתה צריך לעבוד עם HTTPS, אז cURL ישתמש בסיפרת SSL/TLS בצורה שקופה. בעת שימוש ב-cURL בפרויקט שלך, זכור להוסיף אותו למערכת הבניה שלך, כמו CMake או Make.

## ראה גם:
- הדוקומנטציה של cURL: https://curl.se/libcurl/c/
- מדריכים ל-CMake: https://cmake.org/documentation/
- על HTTP/HTTPS ופרוטוקולים: https://developer.mozilla.org/en-US/docs/Web/HTTP
- מדריך Boost.Beast: https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
- מידע על Poco Libraries: https://pocoproject.org/docs/
