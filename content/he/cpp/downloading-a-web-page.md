---
title:                "הורדת עמוד אינטרנט"
html_title:           "C++: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הורדת קובץ אינטרנט היא תהליך שבו מעתקים את קוד המקור של דף אינטרנט מהאינטרנט כדי לאפשר למשתמשים לגשת אליו במחשבים שלהם. התהליך הוא חשוב לתכנותנים כי המידע שנמצא בדפים אינטרנטיים יכול לשמש כמקור עבור מידע או נתונים שהם זקוקים לו בפרויקטים שלהם.

## כיצד לבצע:
מטרתכם היא להוריד את קוד המקור של הדף הזה למחשב שלכם. בשביל לעשות זאת ב-C++ תצטרכו להשתמש בספריית הסטנדרטית של השפה (STL) שנקראת "curl" ליצירת חיבור לאינטרנט וטיפול בקבצים. כאן תוכלו לראות כמה דוגמאות קצרות שמדגימות איך להוריד את הדף בתוך תוכנית C++ שלכם:

```C++
#include <iostream>
#include <fstream>
#include <curl/curl.h>
 
using namespace std;
 
int main()
{
  CURL *curl;
  CURLcode res;
  string url = "https://example.com/";
  ofstream fout("index.html");
 
  curl = curl_easy_init();
  if (curl)
  {
    // יוצר חיבור לאתר
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
 
    // מחזיר את המידע לפונקציה המצוינת
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
 
    // הכוונה לקובץ המקומי שבו ישמור המידע
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &fout);
 
    // מבצע את הבקשה לאתר
    res = curl_easy_perform(curl);
 
    // בורז
    curl_easy_cleanup(curl);
 
    if (res != CURLE_OK)
    {
      // אולי להדפיס או לאחזר הודעה שגיאה
    }
    fout.close();
  }
 
  return 0;
}
```

כתוצאה מהקוד הזה, תקבלו קובץ טקסט בשם "index.html" עם חלק מקוד המקור של הדף שלנו. ניתן לשנות את השם של הקובץ לכל מה שתרצו.

## עומק לשוט:
התהליך של הורדת דפי אינטרנט מקורים נמצא כבר כמה שנים, במקור נעשה באמצעות פנייה בפרוטוקול HTTP על ידי מפענח דפים. היציאה של "curl" נועדה להקל על תהליך כזה כדי לכתוב בקוד פחות. למרבה המזל, פרויקט הספרייה במתכנת אחראי, לכן ההידור והתפעול של "curl" יכול להיות פשוט יותר משאר הפרויקטים. שימוש בפרויקט אחר כמו "wget" תחליף את "curl".

## ראה גם:
"curl" [רשמי אתר אינטרנט](https://curl.haxx.se/libcurl/c/) עם ריפו הדוקומנטציה או אתר אינטרנטת ה-[GitHub](https://github.com/curl/curl) המכיל את הקוד המקורי של הפרויקט.
תמיכה נוספת אפשרית על ידי stackoverflow.com, כמו [הפוסט הזה](https://stackoverflow.com/questions/11472541/how-do-i-download-a-file-using-libcurl-in-c) כדי לייעץ לך על כיצד להשתמש בספריית הסטנדרטית של השפה כ