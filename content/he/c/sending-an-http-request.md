---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:47.498120-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
 שליחת בקשת HTTP זהו פעולה שבה המחשב שלך מבקש נתונים משרת באינטרנט. תוכניתנים עושים את זה כדי לקבל מידע כמו דפי ווב, נתוני API, או כדי לשלוח נתונים לשרת.

## How to: (איך לעשות:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

static size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    printf("%s", (char *)contents);
    return realsize;
}

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();

    return 0;
}
```

### Sample Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (עומק השקעה)
 בעבר, שליחת בקשה ב-C הייתה תהליך מורכב, שדרש חיבורי TCP/IP ידניים לשרת. היום, ספריות כמו libcurl מפשטות את התהליך. ישנם חלופות, כמו ביצוע שימוש ב-sockets בצורה ישירה, אבל libcurl היא פופולרית בשל הנוחות והתמיכה הרחבה בפרוטוקולים. כאשר שולחים בקשת HTTP, יש לנהל את המחזור המלא של החיבור: פתח, שלח, קבל תשובה, ונקה.

## See Also (ראה גם)
- למד עוד על libcurl כאן: [libcurl](https://curl.se/libcurl/)
- מדריך לפרוטוקול HTTP: [HTTP/1.1: The Definitive Guide](http://www.oreilly.com/catalog/9781565925090)
- תיעוד למתכנתי C של Sockets API: [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)
