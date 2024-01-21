---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:01:12.600716-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשות HTTP היא דרך לתקשר עם שרתים ברשת - בעיקר לשלוף או לשלוח מידע. תכניתנים עושים זאת כי זהו היסוד לכל אינטראקציה עם שירותי ווב.

## איך לעשות:
ב־Ruby, שליחת בקשת HTTP יכולה להיות פשוטה. נבחן את הגמ"ח `Net::HTTP`.

```ruby
require 'net/http'
require 'uri'

# יצירת URI מהמחרוזת
uri = URI('http://www.example.com/index.html')

# שליחת בקשת GET
response = Net::HTTP.get_response(uri)

# הדפסת קוד המצב ותוכן התשובה
puts response.code         # לדוגמא: "200"
puts response.body         # תוכן העמוד שבURL המבוקש
```
תוצאת הדוגמא:
```
200
<html>...</html>  # יתכן שיהיה כאן תוכן HTML
```

## עיון מעמיק
לפני שהיו ספריות כמו `Net::HTTP`, תכניתנים נאלצו להשתמש בסקריפטים עם פקודות תחתית-מערכת כדי לבצע פעולות רשת. כיום, יש אלטרנטיבות נוחות יותר כמו `HTTParty` ו`Faraday`. `Net::HTTP` עצמו הוא חלק מהסטנדרט של מתודולוגיות תוכנה של Ruby ומאפשר שליחה תקנית וישירה של בקשות HTTP.

הבנת העבודה מאחורי קלעים של השליחה וקבלת תשובות HTTP מחייבת ידע בפרוטוקולים כמו TCP/IP ו-DNS. קוד התגובה, שנקרא גם קוד סטטוס, חשוב לניתוח המצב של הבקשה - לדוגמא, 200 שהיא הצלחה, 404 שזה לא נמצא, ו-500 שזו טעות של השרת.

## ראה גם
- [Net::HTTP documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [מדריך לHTTParty](https://github.com/jnunemaker/httparty)
- [מדריך לFaraday](https://lostisland.github.io/faraday/)
- [מבוא לפרוטוקול HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)