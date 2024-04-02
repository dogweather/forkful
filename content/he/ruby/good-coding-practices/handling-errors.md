---
date: 2024-01-26 00:56:58.066548-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D6\u05D5 \u05D4\u05EA\u05DB\u05D5\u05E0\u05E0\u05D5\u05EA \u05DC\u05E6\u05E4\
  \u05D9 \u05D4\u05DC\u05D0 \u05E6\u05E4\u05D5\u05D9 \u05D1\u05E7\u05D5\u05D3 \u2013\
  \ \u05E0\u05D9\u05D4\u05D5\u05DC \u05D8\u05E2\u05D5\u05D9\u05D5\u05EA \u05D5\u05D1\
  \u05E2\u05D9\u05D5\u05EA \u05D1\u05D7\u05DF \u05D7\u05E1\u05D3 \u05D5\u05D1\u05DC\
  \u05D9 \u05DC\u05D2\u05E8\u05D5\u05DD \u05DC\u05EA\u05E7\u05DC\u05D5\u05EA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D6\u05E8\u05D9\u05DE\
  \u05EA \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05DB\u05E9\u05D3\u05D1\u05E8\u05D9\
  \u05DD \u05DC\u05D0 \u05DE\u05EA\u05E0\u05D4\u05DC\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:40.217092-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D6\u05D5 \u05D4\u05EA\u05DB\u05D5\u05E0\u05E0\u05D5\u05EA \u05DC\u05E6\u05E4\
  \u05D9 \u05D4\u05DC\u05D0 \u05E6\u05E4\u05D5\u05D9 \u05D1\u05E7\u05D5\u05D3 \u2013\
  \ \u05E0\u05D9\u05D4\u05D5\u05DC \u05D8\u05E2\u05D5\u05D9\u05D5\u05EA \u05D5\u05D1\
  \u05E2\u05D9\u05D5\u05EA \u05D1\u05D7\u05DF \u05D7\u05E1\u05D3 \u05D5\u05D1\u05DC\
  \u05D9 \u05DC\u05D2\u05E8\u05D5\u05DD \u05DC\u05EA\u05E7\u05DC\u05D5\u05EA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D6\u05E8\u05D9\u05DE\
  \u05EA \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05DB\u05E9\u05D3\u05D1\u05E8\u05D9\
  \u05DD \u05DC\u05D0 \u05DE\u05EA\u05E0\u05D4\u05DC\u05D9\u05DD\u2026"
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## מה ולמה?

טיפול בשגיאות זו התכוננות לצפי הלא צפוי בקוד – ניהול טעויות ובעיות בחן חסד ובלי לגרום לתקלות. תכנתים עושים זאת כדי לשלוט בזרימת התהליך כשדברים לא מתנהלים כשורה וכדי לשמר חווית משתמש חלקה.

## איך לעשות:

רובי משתמשת ב־`begin`, `rescue`, `ensure` ו־`end` כדי לטפל בשגיאות. אתה מעטפת את הקוד הרגיש ב־`begin` ו־`end`. אם מתרחשת שגיאה, `rescue` נכנס לפעולה.

```Ruby
begin
  # קוד רגיש נכנס כאן.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "אופס! אי אפשר לעשות את זה: #{e.message}"
ensure
  puts "זה תמיד רץ, יש שגיאה או לא."
end
```

פלט לדוגמא:
```
אופס! אי אפשר לעשות את זה: מחולק ב־0
זה תמיד רץ, יש שגיאה או לא.
```

## התעמקות

בעבר, טיפול בשגיאות בשפות תכנות הולך והתקדם מאוד, כאשר שפות קדומות לעיתים קרובות היו עם מנגנונים גסים או אפילו לא קיימים. הניהול של חריגים ברובי מושרה משפות כמו פייתון וסמולטוק.

חלופות ל־`begin-rescue` ברובי כוללות שימוש ב־`rescue` בהגדרות פונקציה או שימוש ב־`throw` ו־`catch` לזרימת בקרה לא סטנדרטית, למרות שהם לא משמשים לטיפול בשגיאות רגילות.

פרט מעניין אחד: חריגי רובי הם אובייקטים (מופעים של מחלקה `Exception` וצאצאיה), כך שניתן להגדיר מחלקות שגיאה מותאמות אישית ולעשות יותר מסתם לרשום שגיאות – אתה יכול להעביר מצב מורכב ברחבי התוכנית לטיפול יציב יותר בשגיאות.

## ראה גם

- התיעוד של רובי על חריגים וטיפול בשגיאות: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- מדריך מפורט על המתודולוגיה הטובה ביותר לטיפול בשגיאות ברובי: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
