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

# למה
HTTP בסיסיות טוענו מחדש ולמתקדמים הוא הערכה מחדש

## כיצד לעשות
הינו:
```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;
  
  // יצירת עצם CURL לבצע בקשה HTTP
  curl = curl_easy_init();

  // הצהרה על הכתובת
  curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/");

  // ביצוע הפעולה ובדיקת התוצאה
  res = curl_easy_perform(curl);
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
    
  // השבתת עצם CURL
  curl_easy_cleanup(curl);
  
  return 0;
}
```

כפי שאתה רואה, השימוש בספריית libcurl יכול לסייע בשליחת בקשות HTTP פשוטות. בקוד זה, אנו יצרים עצם CURL ומגדירים את הכתובת לכתובת שלאתר הדוגמה. תוצאת הביצוע תודפס במסך והעצם CURL ישוחרר.

## טיפול עמוק
כאשר אנו שולחים בקשת HTTP, אנו בעצם שולחים פנייה לשרת ומבקשים ממנו לעשות פעולה ספציפית. הפנייה מכילה את הכתובת של האתר, מתודת הפעולה (GET, POST, PUT וכו') ופרמטרים נוספים אופציונליים. בנוסף, עלינו לטפל בתצורת הפנייה (headers) ובגוף הבקשה (body), אם הינו זקוקים לכך. כמו כן, אנו יכולים לקבל תגובה מהשרת בצורת קוד תגובה (response code) ותוכן (response body).

# ראה גם
- [libcurl website](https://curl.se/libcurl/)
- [HTTP Request in C++ using libcurl](https://dev.to/shahriyar/https-request-in-c-using-libcurl-421p)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)