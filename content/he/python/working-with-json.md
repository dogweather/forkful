---
title:                "עבודה עם json"
html_title:           "Python: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-json.md"
---

{{< edit_this_page >}}

## למה

ישנם המון יישומים בפייתון המשתמשים בשפת תכנות זו כדי לעבד נתונים מרובי מימדים. לעיתים קרובות, נתוני JSON משמשים כתחליף פשוט וקומפקטי לקבצי CSV. כתבו לנו איך קל זה לעבוד עם JSON בפייתון ותראו לנו מדוע היא היא אפשרות טובה יותר מסוגי נתונים אחרים.

## איך לעשות זאת

העברת נתוני JSON לפורמט פייתון היא קלה ופשוטה מאוד. אנו נשתמש במודול `json` המובנה בפייתון על מנת לטעון ולקרוא נתונים מתוך קובץ JSON.

```Python
import json

# קראו נתונים מתוך קובץ JSON:
with open('file.json') as f:
    data = json.load(f)

# כתבו נתונים לקובץ JSON:
with open('file.json', 'w') as f:
    json.dump(data, f)
```

תוכלו לראות שהשתמשנו בפעולות `json.load()` ו- `json.dump()` על מנת לקרוא ולכתוב נתונים בהתאמה. ניתן גם להשתמש בתוסף נוסף בשם `pprint` על מנת להדפיס ולקרוא נתונים JSON בצורה יותר אנושית.

## חקירה עמוקה

נתוני JSON מורכבים ממבנה פשוט של רשימות ומילונים. ניתן להשתמש בפונקציות שונות כמו `json.dumps()` ו- `json.loads()` על מנת להתמודד עם נתונים מורכבים יותר. כמו כן, ניתן להשתמש בטיפול בשגיאות כדי להתמודד עם נתונים JSON כאשר מתרחשים שגיאות בזמן התהליך.

## ראו גם

- [מדריך לעבוד עם JSON ופייתון](https://realpython.com/python-json/)
- [מסמך הנחיות רשמי על מודול JSON](https://docs.python.org/3/library/json.html)
- [תיעוד על טיפול בשגיאות עם JSON בפייתון](https://docs.python.org/3/library/json.html#module-json)