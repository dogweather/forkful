---
date: 2024-01-20 17:55:28.423369-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05E9\u05D9\u05D8\u05D5\u05EA \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D1\
  \u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05D4\u05D9\u05D5 \u05E4\u05D7\u05D5\u05EA\
  \ \u05D0\u05D9\u05E0\u05D8\u05D5\u05D0\u05D9\u05D8\u05D9\u05D1\u05D9\u05D5\u05EA\
  \ \u05D5\u05D3\u05E8\u05E9\u05D5 \u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\
  \u05D5\u05EA \u05DE\u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05D9\u05D5\u05EA\u05E8\
  . \u05E2\u05DD \u05D4\u05D6\u05DE\u05DF, \u05E8\u05D5\u05D1\u05D9 \u05E4\u05D9\u05EA\
  \u05D7\u05D4 \u05E9\u05D9\u05D8\u05D5\u05EA \u05E7\u05E8\u05D5\u05D0\u05D5\u05EA\
  \ \u05D5\u05E0\u05E7\u05D9\u05D5\u05EA \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05D4\
  \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D4\u05D6\u05D5.\u2026"
lastmod: '2024-04-05T22:50:54.246959-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05E9\u05D9\u05D8\u05D5\u05EA \u05E7\u05E8\u05D9\
  \u05D0\u05EA \u05E7\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05D4\u05D9\u05D5\
  \ \u05E4\u05D7\u05D5\u05EA \u05D0\u05D9\u05E0\u05D8\u05D5\u05D0\u05D9\u05D8\u05D9\
  \u05D1\u05D9\u05D5\u05EA \u05D5\u05D3\u05E8\u05E9\u05D5 \u05DE\u05E0\u05D9\u05E4\
  \u05D5\u05DC\u05E6\u05D9\u05D5\u05EA \u05DE\u05D5\u05E8\u05DB\u05D1\u05D5\u05EA\
  \ \u05D9\u05D5\u05EA\u05E8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות:
```ruby
# קריאה בסיסית של קובץ
File.open('example.txt', 'r') do |file|
  while line = file.gets
    puts line
  end
end

# קריאת קובץ עם תיבוב לבלוק
File.readlines('example.txt').each do |line|
  puts line
end

# קריאה והדפסת התוכן בקובץ בשורה אחת
puts File.read('example.txt')
```

תוצאת הדוגמאות:
```
שורה ראשונה של טקסט
שורה שניה של טקסט
...
```

## היכנסו לעומק:
בעבר, שיטות קריאת קבצי טקסט היו פחות אינטואיטיביות ודרשו מניפולציות מורכבות יותר. עם הזמן, רובי פיתחה שיטות קרואות ונקיות לביצוע המשימה הזו. תוכן הקובץ יכול להיות מועבר למחרוזת או מערך של מחרוזות, בהתאם לצורך.

חלופות נוספות כוללות עבודה עם הגמיש `IO` קלאס או `File.foreach` לקריאה מתקדמת יותר של כל שורה. בבחירת השיטה לקריאת קובץ, חשוב להתחשב בגודל הקובץ ובנפח הזיכרון של המחשב.

## ראו גם:
- [דוקומנטציה של IO ברובי](https://ruby-doc.org/core-3.1.0/IO.html)
- [Class: File (Ruby 3.1.0)](https://ruby-doc.org/core-3.1.0/File.html)
- [Ruby Guides - Read Files](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
