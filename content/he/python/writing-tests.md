---
title:    "Python: כתיבת בדיקות"
keywords: ["Python"]
---

{{< edit_this_page >}}

## מדוע

כתיבת בדיקות בתכנות היא פתרון חכם ויעיל לעומת הושטת קוד בלי סוף. היא מאפשרת לנו לוודא שהקוד שלנו עובד כמו שצריך ולזהות באופן מהיר ויעיל כשמשהו לא עובד כמו שצריך. לכן, כתיבת בדיקות היא מקלה עלינו לתחזק את הקוד שלנו ולהבטיח שהוא ימשיך לעבוד באופן תקין גם בעתיד.

## איך לכתוב בדיקות

```Python
import unittest

class TestCalc(unittest.TestCase):

    def test_add(self):
        self.assertEqual(add(3,4), 7)

    def test_subtract(self):
        self.assertEqual(subtract(5,2), 3)

if __name__ == '__main__':
    unittest.main()
```

הליך כתיבת בדיקות הוא פשוט ונוח. הכי ראשון, אנו יוצרים מחלקת בדיקות עבור הפונקציות שלנו. מכאן, אנו משתמשים בפעולות assert של unittest כדי לוודא שהתוצאות שלנו הן התוצאות הרצויות.

## מעמק כתיבת בדיקות

כדי לכתוב בדיקות יעילות יותר, יש לנו גם תוכלות כמו setUp וְ- tearDown. עם זאת, זה יעזור לנו לארגן את הבדיקות שלנו ולעשות את הקוד יותר קריא ונוח. בנוסף, כדי להפעיל את הבדיקות אוטומטית, אנו יכולים להשתמש בעבודות CI כמו Travis CI או CircleCI.

## ראה גם

- [ניתוח כיסוי בכתיבת בדיקות עם Coveralls](https://coveralls.io/)
- [איך לכתוב טסטים עם PyTest](https://docs.pytest.org/en/stable/)
- [תיעוד המתינה pytest משערך](https://docs.pytest.org/en/latest/reference.html)