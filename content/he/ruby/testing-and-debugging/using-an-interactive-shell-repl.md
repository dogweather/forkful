---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- /he/ruby/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:45.529924-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/using-an-interactive-shell-repl.md"
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
