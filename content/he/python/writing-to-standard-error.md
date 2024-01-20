---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה? ##
כתיבה ל-Standard Error (stderr) היא אופן שבו תוכניות מציגות שגיאות. פרוגרמיסטים עושים זאת כדי להפריד את הודעות השגיאה מפלט רגיל (stdout), ובכך לאפשר ניתוח נוח יותר של בעיות במהלך הרצת התוכנה.

## איך לעשות: ##
```Python
import sys

# כתיבה לפלט התקני
print("זו הודעה רגילה")

# כתיבה לשגיאה התקנית
print("זו הודעת שגיאה", file=sys.stderr)
```
פלט דוגמא:
```
זו הודעה רגילה
זו הודעת שגיאה
```

## עיון עמוק ##
הרעיון מאחורי stderr הוא עתיק, נמצא במערכות UNIX מתחילתן. באשר לאלטרנטיבות, יש למען השוואה גם את רשימות הלוג (logging), שמאפשרות יותר התאמה ומידע. מבחינה טכנית, sys.stderr הוא ערוץ פלט שניתן לשנות אותו ולהפנותו מחדש לקבצים או לטיפולים נוספים.

## ראה גם ##
- [תיעוד Python על sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [מדריך למודול logging ב-Python](https://docs.python.org/3/library/logging.html)