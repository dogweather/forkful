---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת ארגומנטים משורת הפקודה היא האופן בו תוכנית מקבלת נתונים מהמשתמש במהלך ההרצה. המתכנתים משתמשים בזה על מנת למקד את הוראות ההרצה.

## איך לעשות זאת:

Python מספקת את המודולה sys שמאפשר גישה לארגומנטים באמצעות sys.argv:

```Python
import sys

# נדפיס את כל הארגומנטים
for arg in sys.argv:
    print(arg)
```
זהו דוגמא לפלט שיווצר:
```Shell
my_script.py
arg1
arg2
```

## צלילה מעמיקה:

במקור, קריאת ארגומנטים משורת הפקודה הייתה הדרך היחידה להעביר נתונים לתוכנית בהרצה. בשנים האחרונות, נוספו שיטות אלטרנטיביות כמו הקלטה ממשק משתמש גרפי, קבצים, דאטאבסטים, אנדפוינטים API, ועוד. אף על פי כך, קריאת ארגומנטים משורת הפקודה עדיין שימושית מאוד במקרים מסוימים.

מסרעפת sys.argv[0] מכילה את השם של הסקריפט עצמו. הארגומנטים מוסיפים את האינדקסים למערך.

## קישורים למקורות מתאימים:

- [מודול Sys- Python Documentation](https://docs.python.org/3/library/sys.html)
- [קריאת ארגומנטים משורת הפקודה - Real Python](https://realpython.com/python-command-line-arguments/)
- [argparse — מנתח שורת הפקודה bash-style - Python 3.9.7 documentation](https://docs.python.org/3/library/argparse.html)