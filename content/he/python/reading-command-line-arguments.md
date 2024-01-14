---
title:                "Python: קריאת ארגומנטים ממשק שורת הפקודה"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

מדוע כדאי לקרוא ארגומנטי שורת הפקודה בפייתון? כאשר אנו כותבים תוכניות בפייתון, רוב הפעמים נתקלים בצורך לפענח פרמטרים מהפקודה המשתמשת בתוכנית שלנו. בכתבה זו נלמד כיצד לקרוא ארגומנטים מהפקודה ולהשתמש בהם בעזרת פייתון.

## כיצד לעשות זאת

היכן מבקשים מאתנו לקלוט ארגומנטים מהפקודה? ניתן לעשות זאת באמצעות חלק משמעותי בספריית התקשורת בסיסית או בעזרת המודול `argparse` המאפשר לנו לקבוע ארגומנטים צפויים ולדלג לאיתור הארגומנטים שהמשתמש רשם. הנה כמה דוגמאות לשימוש בספריית תקשורת בסיסית ובמודול `argparse` בפייתון:

```Python
import sys

# קריאת ארגומנטים בספריית תקשורת בסיסית
args = sys.argv
print("נתוני הפקודה:", args)

# קריאת ארגומנטים בעזרת אמצעי `argparse`
import argparse

# יצירת אובייקט ArgumentParser
parser = argparse.ArgumentParser(description='קריאת ארגומנטים מהפקודה')

# הגדרת ארגומנטים צפויים לתוכנית
parser.add_argument('--name', help='שם המשתמש')
parser.add_argument('--age', help='גיל המשתמש')
parser.add_argument('--location', help='מיקום המשתמש')

# קבלת הארגומנטים שהמשתמש הכניס
args = parser.parse_args()
print("שם:", args.name)
print("גיל:", args.age)
print("מיקום:", args.location)
```

כלאחרי הרצת תמונה של הפקודה `python reading_command_line_arguments.py --name John --age 25 --location New York` נקבל את הפלט הבא:

```
נתוני הפקודה: ['reading_command_line_arguments.py', '--name', 'John', '--age', '25', '--location', 'New York']
שם: John
גיל: 25
מיקום: New York
```

## התעמקות

הכתבה קיבלה תוכנה מוסכמת: כיצד מסכ