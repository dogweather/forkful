---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא בעצם תהליך נוסף שבו מחוללים מספרים לא צפויים. מתכנתים עשויים ליצור מספרים אקראיים לשם בדיקות, משחקים, סימולציות ועוד. 

## איך עושים את זה:
ניתן להפיק מספרים אקראיים בפייתון באמצעות המודול `random`. להלן מספר דוגמאות:

```Python
import random

# יצירת מספר אקראי בין 1 ל-10
print(random.randint(1, 10))  

# יצירת מספר אקראי מתוך סדרה
print(random.choice(['apple', 'banana', 'cherry', 'date']))

# יצירת מספר אקראי בין 0 ל-1
print(random.random())  
```
## צלילה עמוקה
פייתון משתמשת באלגוריתם מבית "Mersenne Twister" ליצירת מספרים אקראיים. האלגוריתם נפוץ זה שימש להפקת מספרים באלגוריתם גנרי המספרים הראשוני על-פי שיטת "מרסן". אלטרנטיבה למודול `random` היא המודול `numpy.random` שמציע פונקציונליות רחבה יותר.

## ראה גם
- [מדריך התיעוד של מודול random ](https://docs.python.org/3/library/random.html)
- [מדריך התיעוד של מודול numpy.random](https://numpy.org/doc/stable/reference/random/index.html)
- [הסברים נוספים על האלגוריתם "Mersenne Twister"](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [הסברים נוספים על גנרי המספרים הראשוני על-פי שיטת "מרסן"](https://en.wikipedia.org/wiki/Mersenne_prime#Finding_Mersenne_primes)