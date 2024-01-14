---
title:                "C: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה
הורדת עמוד אינטרנט יכולה להיות משימה חשובה בכתיבת תוכניות בשפת סי. זה יכול לאפשר לנו לקבל מידע ונתונים מעמודים אינטרנט ולעבד אותם בתוכנית שלנו. אם אתם מחפשים דרך לאסוף מידע מעמודי אינטרנט, קראו הלאה כדי ללמוד כיצד לעשות זאת בשפת סי.

## כיצד לעשות זאת
אנחנו יכולים להשתמש בספריה הפופולרית "LibCurl" כדי להוריד עמודים אינטרנט לתוך תוכנית סי שלנו. תחילה, נצטרך להתקין את הספריה על ידי הרצת הפקודה הבאה בטרמינל:

```
apt-get install libcurl4-openssl-dev
```

לאחר מכן נוסיף את הספריה לתוכנית שלנו על ידי הוספת השורה הבאה לראשונה של הקוד שלנו:

```
#include <curl/curl.h>
```

כעת, ניצור פונקציה פשוטה שמקבלת כפרמטר את הכתובת של העמוד אינטרנט שברצוננו להוריד. הנה דוגמה לכך:

```
void download_webpage(char* url) {
    CURL *curl;
    FILE *fp;
    char outfilename[FILENAME_MAX];
    strcpy(outfilename, "output.html");
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
}
```

בשורה זו אנחנו יוצרים כתובת לקובץ המכיל את העמודים המורדים. לאחר מכן אנחנו מייצרים את אצווה ה-CURL ומשתמשים בו כדי להגדיר את הכתובת שברצוננו להוריד ואת הקובץ שהוגדרנו לפני כן כמוצא של פקודה זו. לבסוף, אנחנו מבצעים את ההורדה ומסיימים את הספריה.

כעת, נוכל להשתמש בפונקציה זו כדי להוריד עמודים אינטרנט בתוכנית