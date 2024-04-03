---
date: 2024-01-20 17:57:11.563859-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:40.230507-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
```Ruby
# בדוגמא זו התוכנית מדפיסה את כל הארגומנטים שהועברו אליה
ARGV.each do |arg|
  puts arg
end
```
פלט לדוגמא, אם סקריפט זה יופעל עם הפקודה `ruby script.rb תפוח משחק ספר`:
```
תפוח
משחק
ספר
```

## עיון נוסף:
היסטורית, ARGV זו הדרך המקובלת לקריאת ארגומנטים ברוב השפות לכתיבת סקריפטים. זה מאפשר לסקריפטים להיות גמישים ומתאימים למגוון צרכים. חלופות כוללות שימוש בספריות חיצוניות כמו `OptionParser` או `Thor`, שמציעות ממשק נוח יותר לעיבוד אופציות ופלגים. כאשר מעבירים ארגומנטים לתוכנית, רובי מכניס אותם למערך ה-ARGV, ואז אפשר לגשת אליו מאיפה שתרצה בקוד.

## ראה גם:
- [תיעוד Ruby לקריאת ארגומנטים משורת הפקודה](https://www.rubyguides.com/2018/12/ruby-argv/)
- [OptionParser](https://ruby-doc.org/stdlib-2.6.1/libdoc/optparse/rdoc/OptionParser.html)
- [Thor](http://whatisthor.com/)
