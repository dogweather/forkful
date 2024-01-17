---
title:                "יצירת מספרים אקראיים"
html_title:           "Python: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיצול אקראי הוא מהדורה רחבה של תהליך במחשבים שמייצר מספרים אקראיים כחלק מתוכנית מחשב. המטרה העיקרית שלו היא לתת למפתחים אפשרות ליצור מספרים אקראיים לשימוש בפתרון בעיות ובבדיקת קוד.

## איך לעשות זאת:
ליצירת אקראיות ב-Python קיימות שתי אפשרויות עיקריות - מודול random ותוסף NumPy. נהיה נחמדים ונתחיל עם random. הנה דוגמא של כמה קטעי קוד פשוטים עם פלט מתאים:

```Python
import random

# יצירת מספר שלם אקראי בין 1 ל-100
my_number = random.randint(1, 100)
print(my_number)

# יצירת מספר ממשי אקראי בין 0 ל-1
my_float = random.random()
print(my_float)

# בחירת מקור מספר אקראי מתוך רשימה
my_list = [1, 2, 3, 4, 5]
my_choice = random.choice(my_list)
print(my_choice)
```

## לכיוון העומק:
הפיצול האקראי נמצא בתחומי המחשב כבר מתחילת שנות ה-1930 והוא חלק חשוב מכלי המחשב כיום. ישנן גם תוכניות אחרות ליצירת מספרים אקראיים כגון אלגוריתמי סליבנג שנמצאים בתוך חבילת NumPy ועשויים לתת תוצאות טובות יותר ממודול random המובנה של פייתון. בכדי להבין כיצד המודול שלנו פועל, חשוב לדעת שהוא משתמש באלגוריתם מקבוצת Mersenne Twister המאפשר לו ליצור מספרים אקראיים בעזרת פעולות מתמטיות מפתח.

## ראה גם:
1. [דף מסמכי פייתון על random](https://docs.python.org/3/library/random.html)
2. [מדריך בויקיפדיה לפיצול אקראי](https://en.wikipedia.org/wiki/Random_number_generation)
3. [כתבה באתר ג'יטהאביז](https://www.geeksforgeeks.org/generating-random-numbers-in-python/) עם דוגמאות נוספות ליצירת מספרים אקראיים בפייתון.