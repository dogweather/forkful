---
aliases:
- /he/c/downloading-a-web-page/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:50.283852-07:00
description: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D1-C \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\u05D9\u05E9\u05D4\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05EA \u05DC\u05EA\u05D5\u05DB\u05DF \u05E9\
  \u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D3\u05E8\u05DA\
  \ \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D5\u05E9\u05DE\u05D9\u05E8\
  \u05EA\u05D5 \u05DE\u05E7\u05D5\u05DE\u05D9\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05D0\u05D5 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\u05D0 \u05DE\u05E7\u05D5\
  \u05D5\u05DF. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05EA\u05DE\u05D5\u05D3\
  \u05D3\u05D9\u05DD \u05E2\u05DD \u05D6\u05D4 \u05DB\u05D3\u05D9\u2026"
lastmod: 2024-02-18 23:08:53.340647
model: gpt-4-0125-preview
summary: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D1-C \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\u05D9\u05E9\u05D4 \u05EA\
  \u05DB\u05E0\u05D5\u05EA\u05D9\u05EA \u05DC\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\
  \ \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D3\u05E8\u05DA \u05D4\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D5\u05E9\u05DE\u05D9\u05E8\u05EA\u05D5\
  \ \u05DE\u05E7\u05D5\u05DE\u05D9\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\
  \u05D5 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05DC\u05D0 \u05DE\u05E7\u05D5\u05D5\u05DF\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\
  \ \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\
  \u05DD \u05E2\u05DD \u05D6\u05D4 \u05DB\u05D3\u05D9\u2026"
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט ב-C כוללת גישה תכנותית לתוכן של דף אינטרנט דרך האינטרנט ושמירתו מקומית לעיבוד או שימוש לא מקוון. מתכנתים לעיתים קרובות מתמודדים עם זה כדי לצרוך שירותי אינטרנט, לגרד תוכן מהאינטרנט, או להתקשר עם משאבים מקוונים ישירות מתוך האפליקציות שלהם.

## איך לעשות:

כדי להוריד דף אינטרנט ב-C, אחת הדרכים הפופולריות היא באמצעות שימוש בספריית libcurl, ספריית העברת URL בצד הלקוח יעילה וניידת. ודאו שספריית libcurl מותקנת ומקושרת בפרויקט שלכם. הנה דוגמה המדגימה איך להשתמש ב-libcurl כדי להוריד את תוכן דף אינטרנט:

```c
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
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // הפעלת מופע של libcurl בצורה פשוטה
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback לכתיבת הנתונים המתקבלים
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // הגדרת מצביע לקובץ לכתיבת הנתונים

        res = curl_easy_perform(curl); // ביצוע ההורדה של הקובץ
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* ניקוי תמידי */
        curl_easy_cleanup(curl); // ניקוי המופע הפשוט
        fclose(fp); // סגירת זרם הקובץ
    }
    return 0;
}
```
פלט לדוגמה (ללא פלט גלוי בקונסול): קוד זה מוריד את התוכן מה-URL שצוין ושומר אותו בקובץ בשם `downloaded_page.html`. בדקו בספריית התוכנית שלכם עבור קובץ זה כדי לראות את התוכן שהורד.

## צלילה עמוקה:

בהיסטוריה, הורדת תוכן מהאינטרנט ב-C הייתה יותר מסובכת, דורשת תכנות שקעים ידני והתמודדות עם פרוטוקול HTTP. Libcurl מופשטת את המורכבויות הללו, מציעה API גבוהה וחזקה להעברת נתונים דרך האינטרנט.

למרות ש-libcurl מפשטת בקשות HTTP ב-C, שפות תכנות מודרניות כמו Python עם ספריית ה-`requests` שלהם או JavaScript (Node.js) עם ספריות לקוח HTTP שונות עשויות להציע תחביר יותר אינטואיטיבי ותמיכה מובנית ב-JSON ופורמטים אחרים של נתונים הנפוצים בתקשורת אינטרנטית. עם זאת, C ו-libcurl מספקות פתרון ביצועים גבוהים ויציב עבור מערכות שבהן היעילות, השליטה המדויקת או האינטגרציה לקוד C קיים הם קריטיים. כדאי גם לציין ש-C, בשילוב עם libcurl, יכולה לשמש ליותר מהורדת דפי אינטרנט—היא מסוגלת ל-FTP, SMTP והרבה יותר, והופכת אותה לכלי גמיש בארגז הכלים של מתכנת.
