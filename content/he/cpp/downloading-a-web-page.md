---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:43:50.418816-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה לגשת לתוכן שלו דרך הרשת ולשמור אותו במקומית. פרוגרמיסטים עושים זאת כדי לעבד דאטה, בדוק כניסה חיה, או לקחת פרטים.

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
