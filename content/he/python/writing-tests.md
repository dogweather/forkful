---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מתכנתים יוצרים קוד שמוודא שתוכנתם עובדת כשורה. הם עושים זאת כדי להבטיח יציבות, למצוא תקלות לפני לקוחות ולשפר את איכות הקוד.

## איך לעשות:
הנה דוגמא פשוטה ליצירת בדיקה ב-Python באמצעות המודול `unittest`.

```Python
import unittest

def sum(a, b):
    return a + b

class TestSum(unittest.TestCase):
    def test_sum(self):
        self.assertEqual(sum(1, 2), 3)

if __name__ == '__main__':
    unittest.main()
```

הפלט של ריצת הקוד הזה יהיה:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## עיון מעמיק:
### היסטוריה
פיתוח בדיקות החל כשתוכניתים הבינו שבדיקת תוכנה יכולה לפחות חלקית להתאוטמט. זה הוביל לפיתוח של TDD (Test-Driven Development) ו-BDD (Behavior-Driven Development).

### חלופות
`unittest` הוא רק אחד ממספר תשתיות לפיתוח בדיקות ב-Python. חלופות פופולריות כוללות `pytest` ו-`nose2`.

### פרטי יישום
בעת כתיבת בדיקות, מתכנתים צריכים לוודא שהם לא רק מכסים את כל הקוד אלא גם כוללים מקרי קצה ותרחישים לא טריוויאליים.

## ראה גם:
1. התיעוד הרשמי של Python למודול `unittest`: https://docs.python.org/3/library/unittest.html
2. הדרכת Pytest רשמית: https://docs.pytest.org/en/stable/
3. מדריך מעמיק על TDD למתכנתי Python: https://realpython.com/tdd-start-to-finish/
