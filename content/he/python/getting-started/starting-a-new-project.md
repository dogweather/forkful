---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:55.418067-07:00
description: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05D3\u05D5\
  \u05D1\u05E8\u05EA \u05E2\u05DC \u05D4\u05E7\u05DE\u05EA \u05DE\u05E1\u05D2\u05E8\
  \u05EA \u05DE\u05D1\u05D5\u05E0\u05D4 \u05D5\u05E0\u05D9\u05EA\u05E0\u05EA \u05DC\
  \u05EA\u05D7\u05D6\u05D5\u05E7\u05D4 \u05DE\u05D4\u05D4\u05EA\u05D7\u05DC\u05D4\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05D4\
  \u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E7\u05DC \u05DC\u05E7\u05E8\u05D9\
  \u05D0\u05D4, \u05D0\u05D9\u05EA\u05D5\u05E8 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D5\u05E9\u05D9\u05EA\u05D5\u05E3 \u05E4\u05E2\u05D5\u05DC\u05D4,\u2026"
lastmod: '2024-03-13T22:44:38.636619-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1\u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05D3\u05D5\u05D1\
  \u05E8\u05EA \u05E2\u05DC \u05D4\u05E7\u05DE\u05EA \u05DE\u05E1\u05D2\u05E8\u05EA\
  \ \u05DE\u05D1\u05D5\u05E0\u05D4 \u05D5\u05E0\u05D9\u05EA\u05E0\u05EA \u05DC\u05EA\
  \u05D7\u05D6\u05D5\u05E7\u05D4 \u05DE\u05D4\u05D4\u05EA\u05D7\u05DC\u05D4. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E9\u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E7\u05DC \u05DC\u05E7\u05E8\u05D9\u05D0\
  \u05D4, \u05D0\u05D9\u05EA\u05D5\u05E8 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D5\
  \u05E9\u05D9\u05EA\u05D5\u05E3 \u05E4\u05E2\u05D5\u05DC\u05D4,\u2026"
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?

התחלת פרויקט חדש בפייתון מדוברת על הקמת מסגרת מבונה וניתנת לתחזוקה מההתחלה. מתכנתים עושים זאת כדי להבטיח שהקוד שלהם קל לקריאה, איתור שגיאות ושיתוף פעולה, במיוחד ככל שהפרויקט והצוות העובד עליו גדלים עם הזמן.

## איך לעשות:

### יצירת סביבה וירטואלית
סביבה וירטואלית היא ספרייה עצמאית שמכילה את כל הקבצים הביצועיים הנחוצים כדי להשתמש בחבילות שפרוייקט בפייתון יזדקק להם. מומלץ ליצור סביבה וירטואלית לכל פרויקט כדי למנוע סתירות בין תלויות הפרויקט. השתמשו במודול `venv`, שהוא חלק מספריית פייתון הסטנדרטית.

```shell
# החליפו את 'myproject' בשם הפרויקט שלכם
python3 -m venv myproject-env
```

להפעלת הסביבה הווירטואלית:

בחלונות:
```shell
myproject-env\Scripts\activate.bat
```

בUnix או MacOS:
```shell
source myproject-env/bin/activate
```

פלט לדוגמא (הפלט עשוי להשתנות מעט בהתאם למערכת ההפעלה):
```shell
(myproject-env) $
```

### התקנת חבילות
השתמשו ב`pip`, מתקין החבילות של פייתון, להתקנה, שדרוג והסרת חבילות. כך תוכלו להתקין ספרייה צד שלישי פופולרית, `requests`, לביצוע בקשות HTTP:

```shell
pip install requests
```

פלט לדוגמא:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### הקמת מבנה פרויקט
פרויקט פייתון טיפוסי עשוי להיראות כך:

```
myproject/
│
├── myproject-env/    # סביבה וירטואלית
├── docs/             # תיעוד
├── tests/            # בדיקות יחידה ואינטגרציה
│   └── __init__.py
├── myproject/        # קוד מקור של הפרויקט
│   ├── __init__.py
│   └── main.py
├── setup.py          # קובץ ההקמה של הפרויקט
└── README.md         # סקירת הפרויקט
```

### צרו את התוכנית הראשונה שלכם
צרו קובץ `main.py` בתוך הספרייה `myproject`. הנה דוגמא לתוכנית פשוטה:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

הריצו את התוכנית שלכם:

```shell
python myproject/main.py
```

פלט לדוגמא:
```shell
Hello, World!
```

### השתמשו במסגרת עבור פרויקטים גדולים יותר
לפרויקטים גדולים יותר, במיוחד אפליקציות אינטרנט, מסגרות כמו Django או Flask הן בלתי ניתנות להערכה. הנה איך להתקין את Flask וליצור אפליקציית אינטרנט פשוטה של "Hello, World":

```shell
pip install Flask
```

צרו קובץ `app.py` עם התוכן הבא:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

הריצו את אפליקציית Flask:

```shell
flask run
```

פלט לדוגמא:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

נווטו ל-`http://127.0.0.1:5000/` בדפדפן האינטרנט שלכם, ואתם אמורים לראות את ההודעה "Hello, World!".
