---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
מחקרים רבים מראים שהתקשורת ברשת האינטרנט מתבצעת באופן מאובטח יותר כאשר השרתים מחייבים אימות בסיסי כחלק מתהליך התקשורת. אחד הדרכים הפופולריות ביותר לאמת את המשתמש הוא על ידי שליחת בקשת HTTP עם אימות בסיסי.

## איך לעשות זאת
הגבלת הגישה למידע מגיעה עם מערכת אימות בסיסית, בה המשתמש יכול להכניס שם משתמש וסיסמה לקבלת הרשאה למידע. בשפת תכנות C, ניתן לבצע אימות בסיסי זה על ידי הוספת כותרת "Authorization" לבקשת HTTP והצבת מחרוזת מוצפנת המכילה את שם המשתמש והסיסמה. להלן דוגמת קוד לניסוח בקשת HTTP עם אימות בסיסי:

```C
#include <stdio.h>
#include <curl/curl.h>
 
int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
    res = curl_easy_perform(curl);
 
    /* check for errors */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
 
    /* always cleanup */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

כאשר הבקשה מתקבלת על ידי השרת, הנתונים של השם משתמש והסיסמה נבדקים והרשאה למידע ניתנת רק אם הם נכונים.

## שקיפות גבוהה
בין הכתובות המפורסמות ביותר לגישה למידע עם אימות בסיסי ניתן למצוא את API של GitHub ו-Google. כדי להתחבר ל-API הם מומלצים לשלוח בקשת POST יחד עם הגדרת הכותרת "Authorization" ותווים מוצפנים שמכילים את שם המשתמש והסיסמה. לכן, אם אתם מתעסקים עם התכנות במסגרת דרישות אבטחה, כדאי להתחשב באפשרות לשלוח בקשת HTTP