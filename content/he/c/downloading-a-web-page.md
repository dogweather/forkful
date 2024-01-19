---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט משמעה קבלת גישה לתוכן ה- HTML של דף אינטרנט דרך הקוד. מתכנתים מבצעים את זה כדי לנתח נתונים, לבדוק את חווית המשתמש, לבדוק ביצועים ולראות איך עמודים אחרים מתנהגים.

## איך
התוכנית הבאה בשפת C הורדת דף אינטרנט.

```C
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;

    curl = curl_easy_init();

    if (curl) {
        fp = fopen("/tmp/test.txt","wb");
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);

        /* always cleanup */
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

התוצאה שתראו היא דף האינטרנט שהורד מhttp://example.com שמור בקובץ /tmp/test.txt.

## הצלילה לעומק
הורדת דפי אינטרנט הייתה חלק מחיפוש גוגל ובוטים הראשונים שהועלו לרשת. בתיקון מודרני, השימוש בספריות כמו CURL ב-C הופך את זה ליותר פשוט בהרבה. החלופות ל-CURL כוללות Wget ו- Requests ב-Python. לאפשרות זו יש יתרונות נוספים כמו תמיכה במודלים אסינכרוניים וממשק פשוט יותר לשימוש.

## ראה גם
1. התיעוד של [LIBCURL](https://curl.se/libcurl/c/) הוא מקור מצוין למידע נוסף על הספרייה.
2. המדריך של [GNU Wget](https://www.gnu.org/software/wget/) מספק מבט מעמיק על קונצפטים ושימושים להורדת פרטים.
3. מספר מדריכים רבים של [Python Requests](https://realpython.com/python-requests/) מספקים להם גם גישה לשפה ולספרייה.