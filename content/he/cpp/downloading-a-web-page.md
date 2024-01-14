---
title:                "C++: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why (למה):

למה צריך להוריד עמוד אינטרנט באמצעות תכנות C++? ישנם מספר סיבות אפשריות - יכול להיות לצורך איסוף מידע, לניתוח עמוד אינטרנט או פשוט כי זה מאתגר ומעניין למתכנתים.

## How To (כיצד לעשות):

בכדי להוריד עמוד אינטרנט באמצעות תכנות C++ נצטרך להשתמש בספריית הפעלת הרשת מסדונג (Networking) של C++. המטרה העיקרית שלנו היא לשלוט בפעולות של תקשורת ממוחשבת בעזרת פרוטוקולים כמו HTTP. נדגים כמה דוגמאות קוד כדי להמחיש כיצד ניתן להשתמש בספרייה זו:

```C++
#include <iostream>
#include <curl/curl.h> //ספריית CURL המשמשת לפעולות הרשת

//פונקציה שמדפיסה את תוכן העמוד בעזרת פרוטוקול HTTP
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main() {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "https://www.example.com/"; //כתובת האתר שברצוננו להוריד
    char outfilename[FILENAME_MAX] = "example.html"; //שם הקובץ המקומי שנרצה שהאתר יורד לו

    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url); 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl); //מבצע את הבקשה
        curl_easy_cleanup(curl); //סגירת החיבור
        fclose(fp); //סגירת הקובץ
    }
    return 0;
}
```

בתוצאה מן כיתובות הקוד הנמצאות לעיל, נוכל לראות שאנו מפעילים פעולות כמו fopen, fwrite לכתיבה לקובץ ו CURL לבצע את הבקשה לאתר. בנוסף, ניתן להשתמש בפונקציות כמו curl_easy_setopt בשביל לקבוע את פרטי הבקשה, כמו כתובת האתר ופרוטוקול התקשורת.

## Deep Dive (פירוט