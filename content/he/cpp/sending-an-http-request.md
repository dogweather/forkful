---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא תהליך שבו היישום שלך מבקש משאר העולם מידע לאמת להזרים, לבקש ולעדכן נתונים. התכנתים עושים את זה כדי לדבר עם ממשקי API, לשלוט במסדי נתונים ולשתף מידע באופן בטוח.

## איך לעשות את זה:
אפשר לשלוח בקשות HTTP ב-C++ באמצעות ספריות כמו libcurl. הינה דוגמא של קוד ששולח בקשת GET לאתר ומדפיס את התשובה למסוף.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp){
     userp->append((char*)contents, size * nmemb);
     return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        std::cout << readBuffer << std::endl;
    }
    return 0;
}
```

## צלילה עמוקה:
ישנם מספר רב של ספריות מלבד libcurl לשליחת בקשות HTTP ב-C++, כולל POCO, Beast בתוך Boost ו-CPPRESTSDK של Microsoft. אעלה מעט על libcurl: התפתח בראשית שנות ה-2000 והפך לפופולרי בשל קטנותו המשמעותית והמרחק שלו להיות אינדפנדנטי.

## ראה גם:
1. [מידע מפורט על libcurl](https://curl.haxx.se/libcurl/c/)
2. [מעבדה ב-CPPRESTSDK של Microsoft](https://github.com/Microsoft/cpprestsdk)
3. [למד יותר על HTTP](https://www.w3schools.com/tags/ref_httpmethods.asp)