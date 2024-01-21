---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:43:43.628540-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

להוריד דף אינטרנט זה פשוט לקבל את התוכן שלו דרך הרשת. תכניתנים עושים את זה כדי לעבד נתונים, לאסוף מידע, או לבצע בדיקות אוטומציה.

## איך לעשות:

הקוד הבא משתמש בספריית libcurl להוריד דף אינטרנט:

```c
#include <stdio.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

int main(void) {
    CURL *curl;
    CURLcode res;
    FILE *pagefile;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        pagefile = fopen("example.html", "wb");
        if(pagefile) {
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, pagefile);
            res = curl_easy_perform(curl);
            fclose(pagefile);
            if(res != CURLE_OK)
                fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```

תוצאת הדוגמה: קובץ בשם `example.html` יווצר עם התוכן של דף האינטרנט http://example.com.

## צלילה לעומק:

להוריד דף אינטרנט היה קשה יותר בעבר לפני שספריות כמו libcurl הפכו לנגישות. חלופות כוללות שימוש ב-sockets ישירות או בספריות אחרות כמו libhttp או WinInet בחלונות. הסיבוך בפרטים כמו ניהול חיבורים, ניתוב והצפנה הופך את ספריות כמו libcurl לאטרקטיביות למטרה זו.

## ראה גם:

* [מדריך libcurl](https://curl.haxx.se/libcurl/c/)
* [RFC 7230 - Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing](https://tools.ietf.org/html/rfc7230)
* [cURL שימושים בשורת הפקודה](https://curl.haxx.se/docs/manpage.html)
* [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/)