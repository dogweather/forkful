---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:50:15.052609-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים בתכנות היא סיפור על שימוש במספרים שאינם ניתנים לחיזוי לשימושים שונים מבחנים עד אמנות. תכניתנים משתמשים בזה לטסטים, אבטחה, משחקים ושלל יישומים אחרים.

## איך לעשות:
```Python
import random

# יצירת מספר אקראי בין 1 ל-100
random_number = random.randint(1, 100)
print(random_number)
```

פלט דוגמא:
```
42
```

```Python
# יצירת רשימה אקראית של מספרים עם 5 אלמנטים
random_list = random.sample(range(1, 101), 5)
print(random_list)
```

פלט דוגמא:
```
[23, 1, 57, 89, 11]
```

## צלילה עמוקה:
השימוש במספרים אקראיים בתכנות החל עוד בתחילת ימי המחשבים. האתגר הוא ליצור סדרות שנראות "אקראיות", אבל רוב הפונקציות שאנו משתמשים הם למעשה Pseudo-Random, מה שאומר שהם מתחזים לאקראיות. אלגוריתמים כמו Mersenne Twister הם לב המערכת במודול `random` של Python. יש גם אלטרנטיבות כמו `numpy.random` לשימושים מדעיים או `secrets` להגרלות בטוחות יותר.

## ראו גם:
- התיעוד הרשמי של Python למודול `random`: https://docs.python.org/3/library/random.html
- מאמר על אלגוריתמי יצירת מספרים אקראיים: https://en.wikipedia.org/wiki/Random_number_generation
- מודול `secrets` להגרלות בטוחות יותר: https://docs.python.org/3/library/secrets.html