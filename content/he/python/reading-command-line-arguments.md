---
title:                "קריאת פרמטרי שורת פקודה"
html_title:           "Python: קריאת פרמטרי שורת פקודה"
simple_title:         "קריאת פרמטרי שורת פקודה"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

השתמשות בארגומנטים מפקודת השורת פקודה יכולה לשמש לצורך ניתוח ותכנות באופן יעיל ומתמשך. אם אתה מחפש דרך נוחה ומהירה לכתוב תכניות שתתמקדו במידע ממשתמשים, אז קריאת ארגומנטים מפקודת השורת פקודה היא דרך מדהימה להתחיל.

## איך לקרוא ארגומנטים מפקודת השורת פקודה

הכי פשוט ללמוד זאת הוא על ידי דוגמאות פעולה. הנה דוגמאות בפייתון שיעזרו לך להבין איך לקרוא ארגומנטים מפקודת השורת פקודה:

```python
# דוגמא אחת לדגימה
import sys # יבוא המודול sys

# פונקציה פשוטה שמדפיסה את ארגומנט הראשון מהמהיר

def print_argument():
    print(sys.argv[1]) # הדפסת הארגומנט הראשון, argv[0] הוא שם התכנית עצמה

print_argument() # למעשה אנחנו מריצים כאן print_argument עם ללא פרמטרים נוספים
```

הרצה של הקוד הזה עם הארגומנט "Hello world" ממהיר תחזיר את הפלט הבא:

```
Hello world
```

אם נרצה לתת למשתמש להזין את הארגומנט שצריך להדפסה, נוסיף קלט ממשתמש לפני השימוש ב-f argv. כדי לעשות זאת, נשתמש במודול "argparse" המסייע בניתוח וטיפול בארגומנטים מפקודת השורת פקודה. הנה דוגמא לקוד עם המודול argparse:

```python
# דוגמא שנייה לדגימה
import argparse # יבוא המודול argparse

# מילוט את הארגומנט הראשון מהמהיר כפתרונות שנמסרו כארגומנטים

parser = argparse.ArgumentParser()
parser.add_argument("name", help="טקסט להדפסה")
args = parser.parse_args()

# פונקציה פשוטה שמדפיס