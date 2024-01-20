---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# שליחת בקשת HTTP עם אימות בסיסי ב־C++ 
## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא דרך לשלוח בקשת HTTP עם שם משתמש וסיסמה. תכנתים ישתמשו בזה אם הגישה למשאב מוגבלת רק למשתמשים מאומתים.

## כיצד ל:
נשלח בקשה HTTP פשוטה לשרת:

```C++
#include <cpprest/http_client.h>

web::http::client::http_client client(U("http://example.com"));
auto response = client.request(web::http::methods::GET).get();
```

כעת, נוסיף אימות בסיסי:

```C++
web::http::client::http_client client(U("http://example.com"));
web::http::http_request request(web::http::methods::GET);
request.headers().add(L"Authorization", L"Basic base64(username:password)");
client.request(request).then([&](web::http::http_response response){
    std::cout << response.to_string() << std::endl;
}).wait();
```

## צלילה עמוקה
שיטת אימות זו הוא אחד מהגישות הראשונות שהומצאו עם HTTP. זו לא השיטה הבטוחה ביותר, אך היא פשוטה להגדרה ולשימוש. כיום, ישנן שיטות אימות מתקדמות ובטוחות יותר, כמו OAuth. 

אם אתה משתמש בשיטה זו, יש להקפיד תמיד על שילוח בקשות שלך מעל קו HTTPS, על מנת להשאיר את שם המשתמש והסיסמה מוצפנים.

## ר'אף:
ויקיפדיה: [בקשת HTTP](https://he.wikipedia.org/wiki/%D7%91%D7%A7%D7%A9%D7%AA_HTTP)
ספריה cpprest: [תיעוד](https://github.com/microsoft/cpprestsdk/wiki/Getting-Started-Tutorial)