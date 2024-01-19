---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט היא תהליך של שמירת תוכן האינטרנט (כמו דף HTML) על המחשב שלך. מתכנתים מנהלים את תהליך זה כדי לעבד נתונים מהאינטרנט לניתוח התנהגותי, בדיקת חווית משתמש, ועוד.

## איך לעשות:

הנה קוד C++ שמשתמש בספריית libcurl להורדת דף אינטרנט.
```C++
#include <curl/curl.h>
#include <string>

size_t WriteCallback(void *contents, size_t size, size_t nmemb, std::string *s) {
    size_t newLength = size*nmemb;
    s->append((char*)contents, newLength);
    return newLength;
}

std::string DownloadHTML(const char* url) {
    CURL* curl = curl_easy_init();
    CURLcode res;
    std::string s;

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &s);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return s;
}

// מקרה של שימוש: הורדת דף HTML
int main() {
    std::string content = DownloadHTML("http://www.google.com");
    std::cout << content;
    return 0;
}
```

## במעמקים:

1. הקשר היסטורי: הספרייה libcurl שאתה רואה בדוגמה של הקוד הייתה מהראשונות שהפכו את הגישה לאינטרנט לקלה יותר בשפות תכנות C ו־C++. נכון להיום, libcurl היא ספרייה מוכרת ומקובלת שנמצאת בשימוש גם היום.
2. חלופות: ניתן להשתמש בספריות אחרות, כמו Beast ב-Boost, POCO או ספריות מודרניות יותר כמו cpp-httplib. להחלטה יש את התלות בהעדפה האישית, הצרכים של הפרויקט והקביעות להקריב למידה של תכנות חדשה.
3. פרטי ביצוע: libcurl מתמקדת בנוחות של שימוש אך עשויה לא להיות הכי מהירה או הכי יעילה מבחינת ניצול משאבי מערכת. על מנת למקסם את היכולות שלך, תרצה לבדוק ספריות אחרות ולבחון כיצד להשתמש בהן במקרה שלך.

## ראה גם:

- הדרכה להתחלה עם libcurl: https://curl.haxx.se/libcurl/c/
- הספרייה cpp-httplib: https://github.com/yhirose/cpp-httplib
- הספרייה POCO Net: https://pocoproject.org/docs/Poco.Net.html
- הספרייה Beast: https://www.boost.org/doc/libs/1_72_0/libs/beast/doc/html/index.html