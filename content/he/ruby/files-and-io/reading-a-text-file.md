---
title:                "קריאת קובץ טקסט"
aliases:
- /he/ruby/reading-a-text-file/
date:                  2024-01-20T17:55:28.423369-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט ברובי זה לפתוח ולקרוא תוכן שמאוחסן בקובץ. מתכנתים עושים את זה כי לעיתים נתונים חשובים, הגדרות או קוד מקורי נמצאים שם.

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
