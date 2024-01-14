---
title:                "Python: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-tests.md"
---

{{< edit_this_page >}}

## למה

כתיבת בדיקות בתכנות היא חלק חשוב מאוד בתהליך הפיתוח. המתכנתים יוצרים בדיקות כדי לוודא שהקוד שהם כותבים פועל כמו שצריך ואינו גורם לבעיות באפליקציה כולה. ללא בדיקות, הקוד עשוי להכיל בעיות ולגרום לכשלונות באפליקציה שמשתמשים בה.

## איך לעשות זאת

כדי לכתוב בדיקות בשפת פייתון, ניתן להשתמש בספריית unittest המובנית. למשל, ניתן ליצור מחלקה חדשה לבדיקות ולהשתמש בפעולות כמו assertEqual כדי לוודא שתוצאת הפונקציה היא תקינה. הנה דוגמא פשוטה:

```python
import unittest

def multiply(x, y):
    return x * y

class TestMultiply(unittest.TestCase):

    def test_single_value(self):
        result = multiply(2, 5)
        self.assertEqual(result, 10)

    def test_multiple_values(self):
        result = multiply(3, 4)
        self.assertEqual(result, 12)
```

לאחר מכן, יש להריץ את הקוד הזה בעזרת הפקודה unittest.main() כדי לראות את תוצאות הבדיקות. תוצאה לדוגמה תהיה:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

אם כל הבדיקות עוברות בהצלחה, יש לראות את ההודעה OK. אם יש בעיות כלשהן, יופיעה הודעת שגיאה והתוכנית תפסיק לרוץ.

## חפירה עמוקה

כאשר מדברים על בדיקות ב-פייתון, חשוב לדעת גם על סוגי הבדיקות השונים. ישנם שני סוגי בדיקות עיקריים:

1. בדיקות יחידה (Unit testing) - בדיקות שבודקות את מחלקות מסוימות באופן מנומק ובודקות שהן עובדות כמו שאמורות לעבוד.

2. בדיקות עימות (Integration testing) - בדיקות שבודקות את השילוב של מספר מחלקות או פונקציות יחד כדי לוודא שהן עובדות כ