---
title:                "כתיבת בדיקות"
html_title:           "Python: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-tests.md"
---

{{< edit_this_page >}}

כתיבת בדיקות ב־Python – מדריך קצר ופשוט

## מה ולמה?
כתיבת בדיקות (טסטים) היא חלק חשוב מהתהליך של פיתוח תוכנה. זהו תהליך שבו מתבצעת בדיקה של קטעי קוד כדי לוודא שהם עובדים כפי שצריך. התוצאה הסופית היא קוד שפועל באופן נכון ומקפיד על יכולות גבוהות של אמינות ויעילות.

## איך לבצע:
כדי לכתוב בדיקות ב־Python, ניתן להשתמש בפייתון ליבות ה־unittest ו־pytest. ניתן גם להשתמש בספריות נוספות כמו nose ו-doctest. נהלים בדרך כלל פועלים על ידי יצירת קבצי בדיקות והכנת תנאים של ערכים מצופים ובדיקת התוצאה. להלן דוגמאות פשוטות להפעלת כלי בדיקות:

```
# כתיבת מילונים

# מציאת המפתח המקסימלי ב־dict
Python
def test_max_key():
    dictionary = {'a': 1, 'b': 2, 'c': 3}
    assert max(dictionary) == 'c'
```

```
# כתיבת איזורים

# בדיקת שטח איזור
def test_area_calculation():
    height = 5
    base = 10
    expected_result = 25
    assert calculate_area(height, base) == expected_result
```

## בירור מעמיק:
בתחילת זמנו, כתיבת בדיקות הייתה חלק חשוב מהתהליך של פיתוח תוכנה. אין צורך לבצע בדיקות ידניות על כל חלק קטן בפרויקט, מה שהוריד את זמן הפיתוח והעלה את אמינות הקוד. בשנים האחרונות, תורת התכנות הכמעט מכולה קידמה את הקידוד טסטים של הקוד, כדי להיות מבטיחים עם בטוחות נמנו וזוהי ניסויית.

יש גם שיטות אחרות לתחום הבדיקות בפייתון, כגון בדיקות הפעלת Edwin Brady's QuickCheck, המיועדות למציאת בעיות באופן כזה שמאגעות בכיסוי של מטרה עמוקה יותר בצורה ממוחזרת פותחת. שתף פעולה עם בדיקות BDD עם שימוש בפרויקט - תוכניות ה־Gherkin וברירית עם ~־פרסומת־פוססטית־פוססטית־פוססטית־פוסטטטטת־פוססטתת־פוסטתת פרסומות גדולה חסה על קוד כדי לוודא שהוא יכול לבצע עבודה בינונית.

## ראה גם:
* [unittest מסמכי Python](https://docs.python.org/3/library/unittest.html)
* [pytest המדריך הרשמי](http://doc.pytest.org/en/latest/)
* [pytests הדוקומנטציה הרשמית] (http://doc.pytest.org/en/latest/getting-started.html)
* [nose המדריך המצליח ביותר] (https://nose.readthedocs.io/en/latest/)
* [מדריך doctest הרשמי] (https://docs.python.org/3/library/doctest.html)