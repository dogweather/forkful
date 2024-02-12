---
title:                "הורדת דף אינטרנט"
aliases: - /he/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:50.283852-07:00
model:                 gpt-4-0125-preview
simple_title:         "הורדת דף אינטרנט"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
