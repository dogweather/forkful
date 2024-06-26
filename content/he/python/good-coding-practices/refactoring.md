---
date: 2024-01-26 03:38:14.189782-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E0\u05E0\u05D9\
  \u05D7 \u05E9\u05D9\u05E9 \u05DC\u05DB\u05DD \u05D7\u05EA\u05D9\u05DB\u05EA \u05E7\
  \u05D5\u05D3 \u05E9\u05DE\u05D7\u05E9\u05D1\u05EA \u05D5\u05DE\u05D3\u05E4\u05D9\
  \u05E1\u05D4 \u05D0\u05EA \u05D4\u05E9\u05D8\u05D7 \u05D5\u05D4\u05D4\u05D9\u05E7\
  \u05E3 \u05E9\u05DC \u05DE\u05DC\u05D1\u05DF \u05D1\u05D4\u05D9\u05E0\u05EA\u05DF\
  \ \u05D0\u05D5\u05E8\u05DB\u05D5 \u05D5\u05E8\u05D5\u05D7\u05D1\u05D5. \u05D4\u05D9\
  \u05D0 \u05E2\u05D5\u05E9\u05D4 \u05D0\u05EA \u05D4\u05E2\u05D1\u05D5\u05D3\u05D4\
  , \u05D0\u05D1\u05DC \u05D4\u05D9\u05D0 \u05D7\u05D5\u05D6\u05E8\u05EA \u05E2\u05DC\
  \ \u05E2\u05E6\u05DE\u05D4 \u05D5\u05E7\u05E6\u05EA \u05DB\u05D5\u05E9\u05DC\u05EA\
  ."
lastmod: '2024-03-13T22:44:38.649730-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05E0\u05D9\u05D7 \u05E9\u05D9\u05E9 \u05DC\u05DB\u05DD \u05D7\u05EA\
  \u05D9\u05DB\u05EA \u05E7\u05D5\u05D3 \u05E9\u05DE\u05D7\u05E9\u05D1\u05EA \u05D5\
  \u05DE\u05D3\u05E4\u05D9\u05E1\u05D4 \u05D0\u05EA \u05D4\u05E9\u05D8\u05D7 \u05D5\
  \u05D4\u05D4\u05D9\u05E7\u05E3 \u05E9\u05DC \u05DE\u05DC\u05D1\u05DF \u05D1\u05D4\
  \u05D9\u05E0\u05EA\u05DF \u05D0\u05D5\u05E8\u05DB\u05D5 \u05D5\u05E8\u05D5\u05D7\
  \u05D1\u05D5."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לעשות:
נניח שיש לכם חתיכת קוד שמחשבת ומדפיסה את השטח וההיקף של מלבן בהינתן אורכו ורוחבו. היא עושה את העבודה, אבל היא חוזרת על עצמה וקצת כושלת.

```python
# גרסה מקורית
length = 4
width = 3

# חישוב שטח והיקף
area = length * width
perimeter = 2 * (length + width)

print("שטח:", area)
print("היקף:", perimeter)
```

נוכל לרפקטר את זה על ידי אגירת הפונקציונליות לתוך פונקציות, מה שהופך את הקוד למאורגן יותר וניתן לשימוש חוזר:

```python
# גרסה מרופקטרת

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# שימוש
length = 4
width = 3

print("שטח:", calculate_area(length, width))
print("היקף:", calculate_perimeter(length, width))
```

שתי הקטעים יוצרים את אותה התוצאה:
```
שטח: 12
היקף: 14
```

אבל הגרסה המרופקטרת נקייה יותר ומפרידה בין דאגות, מה שמקל על עדכון חישוב אחד ללא השפעה על האחר.

## צלילה עמוקה
ריפקטורינג שורשיו בימים הראשונים של הנדסת תוכנה, כאשר מתכנתים הבינו שקוד יכול—וצריך—להשתפר גם אם הוא כבר "עובד". הספר המכונן של מרטין פאולר "ריפקטורינג: שיפור עיצוב קוד קיים" הביא לידי ביטוי רבים מעקרונות היסוד והטכניקות. הוא אמר בפרסומת, "כל אחד יכול לכתוב קוד שמחשב יכול להבין. מתכנתים טובים כותבים קוד שבני אדם יכולים להבין."

חלופות לריפקטורינג עשויות לכלול כתיבה מחדש של קוד מאפס או ביצוע שינויים קטנים ללא שיפור מערכתי. עם זאת, ריפקטורינג בדרך כלל יעיל יותר מבחינת עלות מאשר כתיבה מחדש ופחות מסוכנת מאשר שינויים אד-הוק. פרטי היישום יכולים להיות ספציפיים לכל פרדיגמה תכנותית; עם זאת, תכנות מונחה-עצמים מתאים במיוחד לריפקטורינג, במיוחד עם טכניקות כמו חילוץ מתודות (כמו הפונקציות `calculate_area` ו-`calculate_perimeter` שלנו), הטמעה, העברת תכונות בין אובייקטים, ושינוי שמות של מתודות או משתנים למען בהירות.

ריפקטורינג בPython עושה שימוש לעיתים בכלים כמו `PyCharm`, שמצויד ביכולות ריפקטורינג פנימיות, או `rope`, ספרייה של Python שתוכננה במיוחד לריפקטורינג. שימוש זהיר בבקרת גרסאות, כמו `git`, במהלך ריפקטורינג מומלץ בחום לעקוב אחרי שינויים בהדרגה.

## ראה גם
למי שרוצה עוד, נסו להיכנס למשאבים הבאים:
- הספר של מרטין פאולר: [ריפקטורינג: שיפור עיצוב קוד קיים](http://www.refactoring.com/)
- ריפקטורינג בPython עם `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- תיעוד ריפקטורינג של PyCharm: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [ריפקטורינג ותבניות עיצוב](https://refactoring.guru/refactoring)
- הרצאות על קוד נקי מאת דוד המלך (Robert C. Martin): [קוד נקי - דוד המלך / שיעור 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
