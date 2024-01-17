---
title:                "שליחת בקשת http"
html_title:           "C++: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שמאפשרת למתכנתים לקבל מידע משרת רשת מסוים. זהו דרך חשובה ליצירת תקשורת בין אתרים ויישומים שונים באינטרנט. מתכנתים מבצעים את הפעולה על מנת לקבל נתונים חדשים או לשלוח פרמטרים לשרת.

## כיצד לעשות זאת:
שליחת בקשת HTTP ב-C ++ נעשית באמצעות חבילת ה-mm. ניתן לבצע את הפעולה עם כמה שורות קוד פשוטות, כפי שניתן לראות בדוגמאות הבאות:

```C++
#include <iostream>
#include <curl/curl.h>
 
int main() {
  CURL* curl;
  CURLcode res;
  curl = curl_easy_init();
 
  if(curl) {
    // כתובת האתר אליו נשלחת הבקשה
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/"); 
    // ייחודית לשם שליחת הבקשה
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.58.0");
    
    // ביצוע הבקשה והדפסת התגובה
    res = curl_easy_perform(curl);
    std::cout << res;
    
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

### תוצאה:
התוכנית ידפיס את הקוד המצביע על הצלחת הבקשה. במקרה הזה, עם קוד 200, הבקשה הושלמה בהצלחה.

## Deep Dive:
ניתן לכתוב פעולת HTTP ב-C ++ על ידי כתיבת כל הפרטים של הבקשה בעצמנו. אולם, חבילת ה-mm מספקת לנו דרכים נוחות יותר לחבר ולשנות מאפייני הבקשה, כגון להוסיף פרמטרים, להגדיר כתובת לבקשה שונה ולא רק. כמו כן, ניתן להגדיר גם תצוגת הבקשה והתגובה ביחד עם הנתונים שנשלחים ומתקבלים.

מכיוון שמדובר בממשק נפוץ מאוד והוא נכתב בשפות תכנות אחרות גם, קיימות גם חבילות אחרות שמציעות את אותן יכולות, כמו Node.js ו-Python.

כדי לשלוח בקשת HTTP ב-C ++ יש לחילופין להשתמש בחבילה שלא אחראית לפורמט הסדר עבודת הרשת, כמו SMTP, IRC, FTP ומספר פרוטוקולים נוספים.

## ראה גם:
- [תיעוד חבילת CURL לשליחת בקשת HTTP ב-C++](https://curl.se/libcurl/c/)
- [מדריך פשוט על שליחת דוא"ל באמצעות CURL](https://curl.se/mailtutorial.html)