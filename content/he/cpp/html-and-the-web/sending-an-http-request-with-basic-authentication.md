---
date: 2024-01-20 18:01:45.335784-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05E1\
  \u05E4\u05E7\u05D9\u05DD \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\
  \u05D9\u05E1\u05DE\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\
  \u05E9\u05D0\u05D1 \u05DE\u05D5\u05D2\u05DF \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D1\u05D8\u05D7\
  \ \u05D0\u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1\u05D9\u05DF\u2026"
lastmod: 2024-02-19 22:04:59.098219
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05E1\
  \u05E4\u05E7\u05D9\u05DD \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\
  \u05D9\u05E1\u05DE\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\
  \u05E9\u05D0\u05D1 \u05DE\u05D5\u05D2\u05DF \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D1\u05D8\u05D7\
  \ \u05D0\u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1\u05D9\u05DF\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
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
