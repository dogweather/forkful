---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:01.143678-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4: Python \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD \u05DE\u05D5\u05D3\u05D5\
  \u05DC \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\u05E9\u05DD `unittest`. \u05D6\u05D5 \u05D4\
  \u05D3\u05E8\u05DA \u05E9\u05D1\u05D4 \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\
  \u05D3\u05D5\u05E7 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E4\u05E9\u05D5\
  \u05D8\u05D4."
lastmod: '2024-03-13T22:44:38.641628-06:00'
model: gpt-4-0125-preview
summary: "Python \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD \u05DE\u05D5\u05D3\u05D5\
  \u05DC \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DC\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\u05E9\u05DD `unittest`."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

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
