---
date: 2024-01-26 04:17:45.529924-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DE\u05E2\u05D8\u05E4\u05EA REPL \u05E9\
  \u05DC Ruby \u05E0\u05E7\u05E8\u05D0\u05EA IRB (Interactive Ruby). \u05E7\u05E4\u05D5\
  \u05E5 \u05E4\u05E0\u05D9\u05DE\u05D4 \u05D5\u05E0\u05E1\u05D4 \u05D0\u05EA Ruby\
  \ \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DE\u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\
  \u05DC \u05E9\u05DC\u05DA."
lastmod: '2024-03-13T22:44:40.206865-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05D8\u05E4\u05EA REPL \u05E9\u05DC Ruby \u05E0\u05E7\u05E8\
  \u05D0\u05EA IRB (Interactive Ruby)."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך ל:
מעטפת REPL של Ruby נקראת IRB (Interactive Ruby). קפוץ פנימה ונסה את Ruby ישירות מהטרמינל שלך:

```Ruby
irb
2.7.0 :001 > puts "שלום, עולם של Ruby!"
שלום, עולם של Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## צלילה עמוקה
הוצג ב-Ruby 1.8, IRB הוא תחמושת בסיסית למשתמשי Ruby. הוא הושרה מהמעטפות האינטראקטיביות של Lisp ו-Python, משלב בין ניסויים לפידבק מיידי. אלטרנטיבות כמו Pry מציעות יותר תכונות כמו הדגשת תחביר וסביבת דיבוג עשירה יותר. ה-IRB עצמו הוא פשוט אך ניתן להרחבה עם ג'מס כמו 'irbtools' להרחבת הפונקציונליות. הדרך שבה IRB מטפל בלולאת קריאה-הערכה-הדפסה היא על ידי קריאת כל שורת קלט, הערכתה כקוד Ruby, ואז הדפסת התוצאה, תהליך זה מחזורי עד ליציאה.

## ראה גם
- [IRB של Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [הג'ם irbtools](https://github.com/janlelis/irbtools)
