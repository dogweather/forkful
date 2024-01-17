---
title:                "שולח דרישת http"
html_title:           "C: שולח דרישת http"
simple_title:         "שולח דרישת http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# מה זה ולמה?
שליחת בקשת HTTP היא פעולה שמאפשרת למחשב לשלוח בקשה לשרת על מנת לקבל מידע או לבצע פעולות. תכנתנים משתמשים בפעולה זו כדי ליצור תקשורת מוצלחת עם שרתים ולבצע פעולות שונות כגון אימות משתמשים או שליחת נתונים לשרת.

# איך לבצע פעולה זו בשפת C:
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    // בניית הבקשה והתאמת הכותרת
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    // שליחת הבקשה והצגת התשובה
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    // הסרת המתחבר
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

# חקירה מעמיקה:
- היסטורית רקע: שליחת בקשת HTTP היא חלק בלתי נפרד מתהליך התקשורת בין מחשבים ושרתים כבר כמה עשורים. זהו דרך פשוטה ויעילה לשלוח ולקבל מידע ומידע מהעולם החיצוני.
- אלטרנטיבות: ישנן כמה דרכים נוספות לשלוח בקשות לשרתים, כגון RESTful API ו-SOAP. כל אחת מהן מתאימה למטרות שונות וזמינות בשפות תכנות שונות.
- פרטים נוספים: בסופו של דבר, שליחת בקשת HTTP היא מסגרת יסודית ונפוצה שמאפשרת למפתחים ליצור תקשורת עם שרתים בצורה יעילה ומתמטית. תוכלו למצוא תיעוד מפורט נוסף על דרך לבצע פעולות נוספות עם הספריה libcurl המשמשת לשליחת בקשות HTTP.

# ראו גם:
- [מדריך מפורט לשליחת בקשות HTTP בשפת C](https://curl.se/libcurl/c/example.html)
- [מאמר מפורט על היסטוריה של שליחת בקשות HTTP](https://searchnetworking.techtarget.com/definition/HTTP)
- [תיעוד מפורט על שימוש בדפדפן curl בכדי לשלוח בקשות HTTP מפקד הטרמינל במערכת UNIX](https://curl.se/docs/manpage.html)