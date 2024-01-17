---
title:                "בדיקת קיום תיקייה"
html_title:           "Python: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

מה ולמה?

בתכנות בשפת פייתון, יצירת קובץ או תיקייה חדשה הוא פעולת יסוד. כשמתכנתים, אנחנו רוצים לוודא שקיימת כבר תיקייה כדי למנוע הרצת שגיאות או לבנות סיגנון מתאים לאפליקצייתנו.

כיצד לבדוק אם תיקייה קיימת?

```
import os
print(os.path.exists("תיקייה"))
```

תוצאה:

```
True
```

מעמקים נוספים

לפני תקופה רבה, התיקיות נפרדו ללא שום יכולת לשמור על ברירת מחדל לקוד פאי ולקוד נאנו במצב MAC או כיבוי שיוקר. תוויות אלו עדיין כמה רקורסיה של קבצי וידאות והעברות של `chmod` התלויות במתחם נפרד של הפועל החומרה המקומי. בשנים האחרונות, המפתחים עברו לעבר תכנות אשר תומך בקריאה כיוונית של המערכות בפייתון, שמתמודדת עם מגוון קנוונות כמו קיצור אבזר של התפקדות מסוג קוד.

ראה גם

- [Doc - os.path](https://docs.python.org/3.8/library/os.path.html)
- [StackOverflow - Checking if a directory exists](https://stackoverflow.com/questions/8933237/how-to-find-out-if-directory-exists-in-python)