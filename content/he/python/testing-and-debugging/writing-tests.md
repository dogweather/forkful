---
title:                "כתיבת בדיקות"
aliases:
- /he/python/writing-tests/
date:                  2024-02-03T19:32:01.143678-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב-Python כוללת יצירת סקריפטים אוטומטיים לאימות נכונות הקוד שלך. מתכנתים עושים זאת כדי לוודא שהפונקציות או המחלקות שלהם פועלות כצפוי תחת תנאים שונים, מה שעוזר לזהות טעויות מוקדם ומקל על תחזוקה ושיפוץ קוד.

## איך לעשות את זה:
Python מגיעה עם מודול מובנה לכתיבת בדיקות בשם `unittest`. זו הדרך שבה אתה יכול להשתמש בה כדי לבדוק פונקציה פשוטה:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "אמור להיות 12")

if __name__ == '__main__':
    unittest.main()
```

כשתריץ את סקריפט הבדיקה הזה, אתה אמור לראות פלט שמציין שהבדיקות שלך עברו (או נכשלו).

לבדיקות יותר מודרניות ומובעות, אתה יכול להשתמש בספרייה צד שלישי כמו `pytest`. ראשית, תצטרך להתקין את זה באמצעות pip:

```shell
pip install pytest
```

לאחר מכן, אתה יכול לכתוב את הבדיקות שלך בדרך פשוטה יותר ללא הצורך לירש מתת-מחלקה כלשהי:

```python
# שמור את זה בקובץ בשם test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "אמור להיות 12"
```

כדי להריץ את הבדיקות שלך עם `pytest`, פשוט בצע:

```shell
pytest test_with_pytest.py
```

אתה אמור לראות פלט מ-pytest המראה את תוצאות הבדיקות שלך.
