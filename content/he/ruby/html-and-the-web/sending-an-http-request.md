---
date: 2024-01-20 18:01:12.600716-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05D5\u05EA HTTP \u05D4\
  \u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\
  \u05E8\u05EA\u05D9\u05DD \u05D1\u05E8\u05E9\u05EA - \u05D1\u05E2\u05D9\u05E7\u05E8\
  \ \u05DC\u05E9\u05DC\u05D5\u05E3 \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\
  \u05D9\u05D3\u05E2. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D9 \u05D6\u05D4\u05D5 \u05D4\u05D9\
  \u05E1\u05D5\u05D3 \u05DC\u05DB\u05DC \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05E6\u05D9\u05D4 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D5\u05D5\
  \u05D1."
lastmod: '2024-02-25T18:49:38.433955-07:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05D5\u05EA HTTP \u05D4\
  \u05D9\u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\
  \u05E8\u05EA\u05D9\u05DD \u05D1\u05E8\u05E9\u05EA - \u05D1\u05E2\u05D9\u05E7\u05E8\
  \ \u05DC\u05E9\u05DC\u05D5\u05E3 \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\
  \u05D9\u05D3\u05E2. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D9 \u05D6\u05D4\u05D5 \u05D4\u05D9\
  \u05E1\u05D5\u05D3 \u05DC\u05DB\u05DC \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05E6\u05D9\u05D4 \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D5\u05D5\
  \u05D1."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
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
