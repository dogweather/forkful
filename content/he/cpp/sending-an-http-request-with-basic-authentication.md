---
title:                "C++: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

לשלוח בקשת HTTP עם אימות בסיסי (basic authentication) נעשה במקרים רבים לצורך ביצוע פעולות ספציפיות באתר אינטרנט, כגון כניסה לחשבון או שליחת טופס.

## איך לעשות זאת

הנה דוגמא של כיצד לשלוח בקשת HTTP עם אימות בסיסי באמצעות שפת תכנות C++:

```C++
// ייבוא הספריה המתאימה לשליחת בקשות HTTP
#include <curl/curl.h> 

int main() {

  // מערך של הערכים שיישלחו עם הבקשה, כאשר הערכים מפורסמים ומוצגים בצורה של זוגות של שם-ערך
  const char* params[] = { "username", "myusername", "password", "mypassword" };

  // נקבע את הכתובת של האתר ואת בקשת הPOST
  CURL *curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/login");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, params);

  // הפעלת הבקשה ובדיקת אם היא הצליחה
  CURLcode res = curl_easy_perform(curl);
  if (res != CURLE_OK) {
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
    return 1;
  }

  // מרכזת את כל תוצאות הבקשה במשתנה שקוראים בו "response_string"
  std::string response_string;
  
  // מידע נוסף על התגובה שנשלחה ייכול להימצא במשתנה שקוראים בו "response_code"
  long response_code;
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
  
  // שחרור המשאבים שנוצרו בתהליך הפעלת הבקשה
  curl_easy_cleanup(curl);

  return 0;
}
```

## ללכת לעומק

על מנת לשלוח בקשת HTTP עם אימות בסיסי באמצעות שפת תכנות C++, צריך להשתמש בספריה מיוחדת שנקראת libcurl. תהליך שליחת בקשת HTTP נעשה באמצעות פונקציות כמו curl_easy_init ו- curl_easy_perform, בהן מציין את הכתובת, את הפרמטרים לשליחה ומעבירים לתוכן התגובה שהתקבלה מהשרת. כדי לאפשר תגובה אינטואטיבית יותר מקודם, חשוב להש