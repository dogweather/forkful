---
title:                "הורדת עמוד אינטרנט"
html_title:           "C: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## על מהומה
אנשים משתמשים בתכניות כמו C כדי לפתח אפליקציות ותוכניות מגוונות, כולל הורדת דפי אינטרנט. הורדה של דף אינטרנט נדרשת כדי לגשת לתוכן מקוון כאשר אינו מחובר לאינטרנט או כאשר אינו מסוגל להתרחק ממכשיר המחשוב שלו. 

## איך לעשות זאת
הנה כמה דוגמאות לכתיבת קוד בשפת C להורדת דף אינטרנט והפקת פלט טקסט:

```C 
#include <stdio.h> 
#include <stdlib.h> 
#include <curl/curl.h> 

int main(void) 
{ 
    CURL *curl; 
    FILE *fp; 
    
    fp = fopen("output.html", "w"); 
    curl = curl_easy_init(); 
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/"); 
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); 
    curl_easy_perform(curl); 
    
    curl_easy_cleanup(curl); 
    fclose(fp); 
    return 0; 
}
```

קוד זה משתמש בספריית libcurl כדי ליצור בקשת רשת לכתובת URL ולהוריד את התוכן במהירות. שימו לב לשורה האחרונה שמסירה כל זיכרון ומאפשרת סגירה נקייה של התכנית. הפלט יופק לקובץ בשם "output.html" בתיקייה הנוכחית.

כאשר התוכן נמצא בזיכרון, ניתן לבצע מספר שינויים כדי לשנות את הפלט הסופי של התוכנית. לדוגמה, ניתן להשתמש בפונקציית printf כדי לבצע טיפול נוסף או לשנות את התוכן של הקובץ.

## טבילה עמוקה
כאשר מורידים דף אינטרנט באמצעות תכניות כמו C, התוכן מאוחסן בזיכרון וניתן לבצע טיפולים נוספים עליו. לדוגמה, ניתן להשתמש בפונקציות כמו sscanf כדי לקרוא את התוכן כטקסט וליישם עליו פעולות אחרות. כמו כן, ניתן לבצע אינדק