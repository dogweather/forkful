---
title:                "Python: מחיקת תווים התואמים דפוס"
simple_title:         "מחיקת תווים התואמים דפוס"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

אז למה כדאי לך למחוק תווים התואמים לתבנית? המחיקה של תווים שאינם רצויים יכולה לסייע לשמירה על קוד נקי וקל יותר לקריאה. גם זה יכול לעזור כאשר אתה מתעסק עם מלל מורכב או נתונים גדולים ואתה רוצה לפשט את המידע לפני שמתחילים בעיבודו.

## איך לעשות את זה

בשפת פייתון ישנם כמה דרכים שבאמצעותן ניתן למחוק תווים התואמים לתבנית. הנה כמה דוגמאות ותמונת מסך של פלט ב-IDLE:

```Python
# דוגמה 1: להשתמש בתכונת replace
text = "hello world"
new_text = text.replace("l", "") 
print(new_text) # יחזיר "heo word"

# דוגמה 2: להשתמש בפונקציית regular expressions
import re
text = "hello world"
new_text = re.sub("[l]", "", text)
print(new_text) # יחזיר "heo word"
```

## ירידה לעומק

כדי להבין יותר טוב כיצד המחיקה של תווים התואמים לתבנית עובדת, ניתן ללמוד עוד על נושאים כמו פונקציות סטרינג, תיבות מלל רגילות ופייתון בכלל. כמו כן, ניתן להעמיק את המבנה של תבניות וללמוד עוד על פונקציות כגון replace ו-sub.

## ראי גם

- [מדריך לפונקציות סטרינג בפייתון](https://realpython.com/python-string-functions/)
- [השתמשו ב-regular expressions בפייתון כדי לטרוח לתבניות בטקסט](https://www.geeksforgeeks.org/python-program-to-check-whether-a-string-matches-a-pattern/)
- [מדריך לתבניות בפייתון](https://www.programiz.com/python-programming/regex)