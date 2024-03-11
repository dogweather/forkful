---
date: 2024-01-20 17:57:11.563859-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05DC\u05E7\u05D7\u05EA \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9 \u05DE\u05D6\u05D9\u05DF \u05DB\u05D7\u05DC\u05E7\
  \ \u05DE\u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DC\u05D4\u05E4\u05E2\u05DC\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D4\u05EA\u05D0\u05DE\u05D4 \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05EA \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D1\
  \u05E2\u05EA \u05D4\u05E8\u05E6\u05EA\u05D4 \u05DC\u05DC\u05D0\u2026"
lastmod: '2024-03-11T00:14:13.742852-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05DC\u05E7\u05D7\u05EA \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D4\
  \u05DE\u05E9\u05EA\u05DE\u05E9 \u05DE\u05D6\u05D9\u05DF \u05DB\u05D7\u05DC\u05E7\
  \ \u05DE\u05D4\u05E4\u05E7\u05D5\u05D3\u05D4 \u05DC\u05D4\u05E4\u05E2\u05DC\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D4\u05EA\u05D0\u05DE\u05D4 \u05D3\u05D9\u05E0\
  \u05DE\u05D9\u05EA \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D1\
  \u05E2\u05EA \u05D4\u05E8\u05E6\u05EA\u05D4 \u05DC\u05DC\u05D0\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא לקחת מידע שהמשתמש מזין כחלק מהפקודה להפעלת התוכנית. תכניתנים עושים זאת כדי לאפשר התאמה דינמית של התוכנית בעת הרצתה ללא צורך בשינויי קוד.

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
