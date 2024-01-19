---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספריה קיימת היא תהליך בו משתמש המחשב בודק אם ספריה מסוימת קיימת במערכת. זה חשוב כדי למנוע שגיאות בהרצה, כאשר התוכנה מנסה לגשת אל מיקום שאינו קיים.

## איך לבדוק:
הקוד הבא בשפת Ruby מדגים איך בודקים אם ספריה קיימת.
```Ruby
Dir.exists?("/path/to/directory")
```
אם הספריה קיימת הקוד מחזיר ערך של True, אחרת הוא מחזיר False.

## היכנס עמוק יותר:
בעבר, בשפת Ruby, השימוש היה ב-mkdir מהמחלקה Dir ליצירה של ספריה. אך הפקודה לא מקנה את האפשרות לבדוק אם הספריה ממש נוצרה. עבור זאת הומצאה הפקודה exists?. אפשרויות נוספות הן גם `File.exists?` או `File.directory?`. לגבי אשר כל אחת מהן נמצאת במעמד שונה של "אידאומטיות" בשפת Ruby.

## ראה גם:
- [מסמך הגדרה של מחלקת Dir בשפת Ruby](https://ruby-doc.org/core-2.7.1/Dir.html)
- [מדריך לשימוש במחלקת File בשפת Ruby](https://ruby-doc.org/core-2.2.0/File.html)
- [דיון בStackoverflow על הבחינה בין File.directory? ל-Dir.exists?](https://stackoverflow.com/questions/2108727/which-in-ruby-file-exists-vs-file-directory)