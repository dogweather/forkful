---
aliases:
- /he/ruby/using-an-interactive-shell-repl/
date: 2024-01-26 04:17:45.529924-07:00
description: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 REPL (Read-Eval-Print Loop - \u05DC\
  \u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\u05E2\u05E8\u05DB\
  \u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\
  \u05DA \u05DC\u05D1\u05D7\u05D5\u05DF \u05E7\u05D5\u05D3 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D0\u05DE\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E1\u05D5\
  \u05EA, \u05DC\u05E0\u05E4\u05D5\u05EA \u05D1\u05D0\u05D2\u05D9\u05DD,\u2026"
lastmod: 2024-02-18 23:08:53.392708
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 REPL (Read-Eval-Print Loop - \u05DC\
  \u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\u05E2\u05E8\u05DB\
  \u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\
  \u05DA \u05DC\u05D1\u05D7\u05D5\u05DF \u05E7\u05D5\u05D3 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D0\u05DE\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E1\u05D5\
  \u05EA, \u05DC\u05E0\u05E4\u05D5\u05EA \u05D1\u05D0\u05D2\u05D9\u05DD,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
מעטפת אינטראקטיבית, או REPL (Read-Eval-Print Loop - לולאת קריאה-הערכה-הדפסה), מאפשרת לך לבחון קוד בזמן אמת. מתכנתים משתמשים בה כדי לנסות, לנפות באגים, וללמוד את הפרטים הדקים של Ruby מבלי ליצור סקריפטים מפוארים.

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
