---
title:                "התחלת פרויקט חדש"
aliases:
- /he/python/starting-a-new-project/
date:                  2024-01-20T18:05:01.538412-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/starting-a-new-project.md"
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
