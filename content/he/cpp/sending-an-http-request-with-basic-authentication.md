---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:45.335784-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו אנו מספקים שם משתמש וסיסמה כדי לגשת למשאב מוגן באינטרנט. תכניתנים עושים זאת כדי לאבטח את תקשורת הנתונים בין הלקוח לשרת.

## איך לעשות:
בדוגמה הבאה, אנחנו משתמשים בספריית cURL של C++ כדי לשלוח בקשה עם אימות בסיסי.

```C++
#include <curl/curl.h>
#include <iostream>
#include <string>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        std::string username = "your_username";
        std::string password = "your_password";
        std::string credentials = username + ":" + password;
        
        curl_easy_setopt(curl, CURLOPT_URL, "https://your.api/endpoint");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, credentials.c_str());

        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        }
        
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return 0;
}
```

אם האימות הצליח, התוכנית תקבל תשובה מהשרת שהיא מנסה לגשת אליו. אם לא, תודפס הודעת שגיאה.

## עיון מעמיק:
אימות בסיסי ב-HTTP הוא שיטה ותיקה שמשתמשת בקידוד Base64 למידע על שם משתמש וסיסמה. למרות זאת, היא אינה נחשבת לבטוחה מאוד כיוון שניתן לפענח את קידוד Base64 בקלות. בהקשרים שבהם אבטחת הנתונים מהווה עניין קריטי, מפתחים עשויים לבחור בשיטות אימות מתקדמות יותר כגון OAuth.

השימוש ב-cURL ב-C++ נפוץ כיוון שהספריה רוחבית פלטפורמה ותומכת במגוון גדול של פרוטוקולים. זה מקל על כתיבת קוד ששולח בקשות רשת באופן יעיל ובטיחותי.

אם אתם מחפשים אלטרנטיבות, ספריות כמו Boost.Beast (מבוססת על Boost.Asio) ו-Poco יכולות לשמש אפשרויות. הן מאפשרות לך ליצור בקשות HTTP בצורה מורכבת יותר, אך עם יכולת קריאה וניהול קוד טובה יותר.

## ראה גם:
- [cURL Library Official Site](https://curl.se/libcurl/)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
- [Boost.Beast Documentation](https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html)
- [Poco Libraries](https://pocoproject.org/docs/index.html)