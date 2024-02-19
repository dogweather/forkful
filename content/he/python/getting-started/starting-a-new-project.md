---
aliases:
- /he/python/starting-a-new-project/
date: 2024-01-20 18:05:01.538412-07:00
description: "\u05DB\u05E9\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD \u05DE\u05EA\u05D7\u05D9\
  \u05DC\u05D9\u05DD \u05D1\u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9\
  , \u05D4\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05DE\u05E1\u05D2\u05E8\u05EA\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05E7\u05D5\u05D3 \u05E9\u05D9\u05E4\
  \u05EA\u05D7\u05D5. \u05E2\u05E9\u05D9\u05D9\u05EA \u05D6\u05D0\u05EA \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05DD \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC\
  \ \u05E2\u05DD \u05E7\u05E8\u05E7\u05E2 \u05E7\u05E8\u05D5\u05D9\u05D4 \u05D5\u05DC\
  \u05D0\u05E8\u05D2\u05DF \u05D0\u05EA \u05E8\u05E2\u05D9\u05D5\u05E0\u05D5\u05EA\
  \u05D9\u05D4\u05DD."
lastmod: 2024-02-18 23:08:52.437308
model: gpt-4-1106-preview
summary: "\u05DB\u05E9\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD \u05DE\u05EA\u05D7\u05D9\
  \u05DC\u05D9\u05DD \u05D1\u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9\
  , \u05D4\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05DE\u05E1\u05D2\u05E8\u05EA\
  \ \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05E7\u05D5\u05D3 \u05E9\u05D9\u05E4\
  \u05EA\u05D7\u05D5. \u05E2\u05E9\u05D9\u05D9\u05EA \u05D6\u05D0\u05EA \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05DD \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC\
  \ \u05E2\u05DD \u05E7\u05E8\u05E7\u05E2 \u05E7\u05E8\u05D5\u05D9\u05D4 \u05D5\u05DC\
  \u05D0\u05E8\u05D2\u05DF \u05D0\u05EA \u05E8\u05E2\u05D9\u05D5\u05E0\u05D5\u05EA\
  \u05D9\u05D4\u05DD."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?
כשמפתחים מתחילים בפרויקט חדש, הם יוצרים מסגרת בסיסית לקוד שיפתחו. עשיית זאת מאפשרת להם להתחיל עם קרקע קרויה ולארגן את רעיונותיהם.

## איך לעשות:
על מנת להתחיל פרויקט חדש בפייתון, יש כמה דברים שצריך לעשות:

```Python
# יצירת תיקייה חדשה לפרויקט
mkdir my_new_project
cd my_new_project

# הגדרת סביבת עבודה וירטואלית
python -m venv venv
# הפעלת הסביבה הווירטואלית ל-Windows
venv\Scripts\activate.bat
# או עבור Unix או MacOS
source venv/bin/activate

# התקנת תלויות (לדוגמא, התקנת Flask המסגרת לאינטרנט)
pip install flask

# יצירת קובץ app.py
echo "from flask import Flask" > app.py
echo "app = Flask(__name__)" >> app.py
echo "@app.route('/')" >> app.py
echo "def hello():" >> app.py
echo "    return 'Hello, World!'" >> app.py

# ריצת הפרויקט
python app.py
```

תוצאה מהרצה:
```
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

## צלילה עמוקה
התחלת פרויקט חדש היא מקובלת מאז שכתיבת קוד התחילה. הרעיון הוא להפריד בין הפיתוח לבין הסביבות השונות: פיתוח, בדיקה ופרודקשן. עם Python, המומלץ הוא להשתמש בסביבה וירטואלית כדי למנוע ערבוב בין תלויות של פרויקטים שונים.

לפני שנהפוך את זה למסובך, למעשה, היו ועדיין יש דרכים אחרות לעשות את זה. כגון דוקר, פיפנב, או פואטרי. הן מספקות יכולת ניהול תלויות מתקדמת ויותר מתקדמת.

עבור לצד היסטורי ותראה שמפתחים התחילו רק עם קובץ טקטסט רגיל וקומפיילר. יותר ויותר כלים וטכניקות פותחו כדי לעזור למפתחים להתרכז במה שחשוב - פיתוח הקוד עצמו.

## ראה גם
- [המסמכים הרשמיים של Python לסביבה וירטואלית](https://docs.python.org/3/library/venv.html)
- [Flask, מסגרת אינטרנט](https://flask.palletsprojects.com/)
- [מדריך למתחילים ב-Poetry](https://python-poetry.org/docs/basic-usage/)
- [מדריך ל-Docker עבור מפתחי Python](https://docker-curriculum.com/#dockerizing-a-python-web-app)
