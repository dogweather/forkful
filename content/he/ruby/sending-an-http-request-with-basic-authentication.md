---
title:                "שליחת בקשת http עם אימות בסיסי בתכנות מחשבים."
html_title:           "Ruby: שליחת בקשת http עם אימות בסיסי בתכנות מחשבים."
simple_title:         "שליחת בקשת http עם אימות בסיסי בתכנות מחשבים."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה
שליחת בקשת HTTP עם אימות בסיסי היא טכניקה נפוצה בתכנות רשתות וכוללת יתרונות רבים, כגון אבטחה נוחה ופשוטות ביישומים מרוחקים.

## איך לעשות זאת
```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://www.example.com")
http = Net::HTTP.new(uri.host, uri.port)

request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("username", "password")

response = http.request(request)
puts response.body
```
ניתן לראות בדוגמה זו כמה פשוט את השליחה של בקשת HTTP עם אימות בסיסי באמצעות ספריית ההתחברות של Ruby, Net::HTTP. פשוט ניתן ליצור אובייקט כתובת אינטרנט ואובייקט HTTP ולהשתמש בפעולת basic_auth כדי להוסיף מידע התחברות, ולאחר מכן לשלוח את הבקשה עם פעולת request. התוצאה הנמצאת בתוך response.body היא תשובת השרת המעודכנת.

## העמקת הנושא
כאשר נשלחת בקשת HTTP עם אימות בסיסי, שם המשתמש והסיסמה מועברים במידת האפשר בקוד Base64, אשר הוא פורמט תוכן המאפשר הצפנה של נתונים מסוג טקסט. זה נועד לא להיות אופציה יציבה יותר מכיוון שבסרטן של המחשב עשו ניתוח כדי להגיע לאותו הסיסמה כמו בקשה אחרת, ולא להגיע לספקים לחיסור טיפולים בחולים באופן כללי. מעבר לכך, כמו בכל שיחה באמצעות דרמומונים, הרשת האינטרנט יהלום יהיה לידה מושחקת להצביע על המכשיר שמאפשר הגנה על המתקן ומשום שמים לב לאחת דרגת הידידות שתורשת הכלאה נמש",
 "Net::HTTP - ספריית התחברות של Ruby
- https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- https://www.rubyguides.com/2018/02/ruby-http-request/
-