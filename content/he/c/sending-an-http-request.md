---
title:                "C: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

אנשים יכולים לרצות לשלוח בקשת HTTP מכיוון שזהו אמצעי נוח להתקשרות עם שרתים ולקבלת מידע מהם.

## איך לעשות זאת

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    // ייבוא קובץ כותרת לביצוע קריאות HTTP
    #include <curl/curl.h>

    // משתנה עבור כתובת האתר שבסוף הנספח נשלח אליו בקשת HTTP
    char *url = "https://www.example.com";

    // משתנה עבור נתוני הסטטוס של הבקשה
    CURLcode res;

    // יצירת משתנה עבור הספריה של curl לניהול הבקשות
    CURL *curl;

    // התחלת פעולת curl ובקשת GET לכתובת האתר
    curl = curl_easy_init();

    if (curl) {
        // הגדרת האתר לפעולת curl
        curl_easy_setopt(curl, CURLOPT_URL, url);

        // ייצוב הבקשה על ידי הכנסת "1" במשתנה לבקשה GET
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

        // ביצוע הבקשה ואחסון תוצאת הבקשה ב-mres
        res = curl_easy_perform(curl);

        // אם הבקשה נכשלה, תציג את השגיאה
        if (res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n");
            curl_easy_strerror(res);
        }
        // כלל הסטורציה
        curl_easy_cleanup(curl);
    }
    // סיום תכתובת האתר
    free(url);

    return 0;
}
```

פלט משמעותו שהקוד שמעל, במידה והבקשה נענתה בהצלחה, ישלח את האתר שמכיל את התוכן של הבקשה.

## חקירה מעמיקה

כדי להבין יותר על תהליך שליחת בקשת HTTP במקוד C, כדאי לעיין בתיעוד של ספריית curl ולהתחיל להשתמש בפקודות כמו `CURLOPT_URL` על מנת לשלוח את כתובת האתר שבסופו ישלח אליו הבקשה. כמו כן, יש לוודא כי המתאם לחיבור הרשת curl נמצא מותקן על המחשב.

## ראו גם

- [ספריית curl הרשמית על תקשורת בינהן](https://curl.haxx.se/libcurl/)
- [ד