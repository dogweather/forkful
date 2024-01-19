---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה זה & למה:

שליחת בקשת HTTP היא שיטה בה מחשב נותן לשרת לדעת שהוא רוצה לעבוד עם מידע מסוים. למתכנתים זה מאוד שימושי כי זה מאפשר להם לגשת למשאבים מרחוק על שרתים אחרים.

## איך לבצע:
```C
#include <stdio.h>
#include <curl/curl.h>
 
int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl_global_init(CURL_GLOBAL_DEFAULT);
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
 
    /* Perform the request, res will get the return code */ 
    res = curl_easy_perform(curl);
    /* Check for errors */ 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
     
    /* always cleanup */ 
    curl_easy_cleanup(curl);
  }
 
  curl_global_cleanup();
 
  return 0;
}
```
תוצאות הדוגמה:
```
curl_easy_perform() failed: Couldn't resolve host name
```
## דיבור עמוק:
השיטה הזו משמשת כבר שנים רבות, מאז שה-Microsoft אימצו את ה-HTTP version 1.0 בשנת 1996. לשליחת בקשת HTTP יש גם חלופות, כמו השימוש ב-FTP או ב-SFTP, אבל הן לא נפוצות כמו ה-HTTP. פרטי ההפעלה כוללים השתמש ב- "CURLOPT_URL" כדי לקבוע את הכתובת שאליה הבקשה נשלחת. 

## ראה גם:
[Curl מדריך שימוש](https://curl.haxx.se/libcurl/c/)
[מאגר מידע של HTTP](https://developer.mozilla.org/he/docs/Web/HTTP/Overview)