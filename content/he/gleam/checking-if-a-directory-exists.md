---
title:                "בדיקת קיום תיקייה"
html_title:           "Gleam: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
כתיבת תוכניות בצורה יעילה ומדויקת דורשת מאתנו להיות מודעים לכללי התכנות, למערכת הפעולה ולפקודות הספציפיות. בכתבי התכנות תמיד יהיה לנו צורך לבדוק אם תיקייה קיימת או לא, ובכתיבת תוכניות בשפת גליאם זה משתתף במלאכה לולאכת ומהיר יותר.

## How To
בשפת גליאם ניתן לבדוק אם תיקייה קיימת על ידי שימוש בפקודת `File.system?` ומספר פרמטרים כגון נתיב התיקייה ואם ניסים לבדוק אם התיקייה קיימת עם `is_dir = true`. לדוגמה, ננסה לבדוק אם התיקייה "documents" קיימת בנתיב "users/user1", נכתוב את הקוד הבא:

```Gleam.
let exists = File.system?("users/user1/documents", is_dir = true)
```

אם התיקייה קיימת נקבל כתוצאה `true`, ואם התיקייה לא קיימת נקבל כתוצאה `false`.

## Deep Dive
כאשר אנו משתמשים בפקודת `File.system?`, היא מחזירה ערך בוליאני - `true` אם הנתיב קיים ומכיל פרמטר שאומר אם קיים גם תיקייה או קובץ. כאשר אנו משתמשים בפרמטר האופציונלי `is_dir = true`, נוכל לבצע בדיקה רק לתיקיות קיימות.

## See Also
* [פרק 2: בדיקות תנאים](https://gleam.run/book/basics/conditionals.html)
* [פקודת המערכת File בשפת גליאם](https://gleam.run/core/file.html)