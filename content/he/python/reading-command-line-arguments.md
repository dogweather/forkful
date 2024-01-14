---
title:                "Python: קריאת ארגומנטים משורת הפקודה"
simple_title:         "קריאת ארגומנטים משורת הפקודה"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##למה:
קבלת כרטיסי מפקד מביאה ערך מוסף לתוכניות שלנו ומאפשרת לנו להתאים את הקוד שלנו למידרגות שונות של התוכנית. כמו כן, קבלת כרטיסי מפקד מאפשרת לנו להתאים את התוכנית לצרכים של משתמשי התוכנית.

##איך לעשות את זה:
לקבלת כרטיסי מפקד בפייתון ניתן להשתמש בפונקציה sys.argv. הנה דוגמא פשוטה של כיצד לקבל כרטיסי מפקד מהמשתמש:

```Python
import sys

print("הכנס את הפרמטרים:")
print(sys.argv)
```
והנה התוצאה שנקבל:
```Python
הכנס את הפרמטרים:
['program_name.py', 'arg1', 'arg2', 'arg3']
```

בדוגמא זו, הפקודה שהרצנו היא "python program_name.py arg1 arg2 arg3", כאשר "program_name.py" הוא שם התוכנית ו-"arg1", "arg2" ו-"arg3" הם הפרמטרים שהוספנו בכרטיסי המפקד.

##מעמקים נמוכים:
בנוסף לכיצד לקבל כרטיסי מפקד, ניתן גם לשלוט על כיצד הם ייוצגו בפונקציה sys.argv. ניתן להגדיר את סדר הפרמטרים שיימסרו ואת הנתיב לתוכנית הראשית. למשל, ניתן להשתמש בספריית argparse המאפשרת לנו להגדיר פרמטרים בצורה מדויקת ולהכיל בהם בדיקת תקינות.

##ראה גם:
למידע נוסף על שימוש בכרטיסי מפקד בפייתון, ניתן להסתכל על המדריך המפורט של מודול ה-sys ב[אתר הרשמי של פייתון](https://docs.python.org/3/library/sys.html) וב[מדריך המפקדים של argparse](https://docs.python.org/3/howto/argparse.html).