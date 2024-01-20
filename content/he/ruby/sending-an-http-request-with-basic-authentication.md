---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
קודם כל, נפעיל בקשה HTTP עם אימות בסיסי הוא  העברת נתונים עם בעזרת ה-API HTTP בעזרת שם משתמש וסיסמה. מתכנתים בדרך כלל משתמשים בעזרת אימות HTTP בסיסי כאשר יש להם API שגורסאות משתמשים מאובטחים. 

## מדריך שלב בשלב:
קודים המציגים את העקרון ברובי:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/index.html')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body
```
כלולים שבו:
- הגדרת ה-URI שלך.
- יצירת בקשה `Get` חדשה.
- הגדרת שם המשתמש והסיסמה על הבקשה.
- פתיחת חיבור HTTP ושליחת הבקשה.
- הדפסת תוצאת התגובה.

## ירידה לעומק
אימות HTTP בסיסי הוא פתרון אימות שהוצג ב-1996, ומכונה כ"בסיסי" מאחר והוא סופק עם כל ממשקי ה-API HTTP. יתרונו הוא שהוא פשוט וקל לשימוש, אך מהצד השני, האימות לא מאובטח במיוחד.

החלופות לאימות בסיסי כוללות אימות הדבק ואימות Bearer Token, שהם מאובטחים יותר, אך יכולות להיות מעט מורכבות יותר.

כאשר אנחנו משתמשים באימות HTTP בסיסי, השם משתמש והסיסמה נשלחים לשרת כקודלק בבסיס64, ניכר. אימות זה לא מבצע הצפנה, לכן אינו מומלץ כאשר אתה שולח מידע רגיש.

## קישורים רלוונטיים
- [דף הוויקיפדיה של HTTP basic authentication](https://he.wikipedia.org/wiki/%D7%90%D7%99%D7%9E%D7%95%D7%AA_%D7%91%D7%A1%D7%99%D7%A1%D7%99_HTTP)
- [מדריך Net::HTTP של Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [מסמך IETF של HTTP Basic Authentication](https://tools.ietf.org/html/rfc7617)