---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:29.460107-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא שיטה לגישה מאובטחת למשאבים ברשת - אתה מעביר שם משתמש וסיסמה בראש הבקשה. תוכניתנים מבצעים זאת כדי להבטיח כי רק משתמשים מורשים יוכלו לראות או לשנות מידע.

## איך לעשות:

כדי לשלוח בקשת HTTP עם אימות בסיסי ב-C, ניתן להשתמש בספריית libcurl. הנה דוגמה לקוד:

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourwebsite.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");

        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```

הפלט יהיה תלוי בתגובת השרת לבקשה.

## טבילה עמוקה

שיטת אימות ה-HTTP הבסיסית קיימת כבר זמן רב, והיא חלק מהמפרט HTTP 1.0 מאז 1996. ישנן שיטות אימות מאובטחות יותר כגון OAuth, אבל אימות בסיסי עדיין נפוץ עקב פשטותו. חשוב לזכור שהסיסמאות אמורות להישלח באופן מאובטח (HTTPS ולא HTTP) כדי למנוע חשיפת מידע רגיש. מימוש נכון צריך לכלול גם בדיקת התגובה הנקבלת וניהול שגיאות בצורה ראויה.

## ראו גם

- מסמכי libcurl: https://curl.se/libcurl/c/
- מידע נוסף על אימות בסיסי ב-HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- מידע על אימות מאובטח יותר כגון OAuth: https://oauth.net/