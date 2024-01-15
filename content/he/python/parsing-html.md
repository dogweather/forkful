---
title:                "ניתוח אתרים ב- HTML"
html_title:           "Python: ניתוח אתרים ב- HTML"
simple_title:         "ניתוח אתרים ב- HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
---

{{< edit_this_page >}}

## למה

ניתן להשתמש בפירסום HTML כדי להבין ולעבד מידע מכל אתר אינטרנט. השימוש בכלי זה יעיל ביותר כאשר רוצים לאסוף מידע מסוים מאתר אינטרנט או לבנות כלי לאנליזה שיכול לסייע בעבודה מאתגרת יותר.

## איך לבצע

תחילה, ניתן להתקין גרסה נוחה של פייתון (שנוכל להשתמש בה עכשיו, בגרסה האחרון לכתיבה של המאמר). לבדוק באם כבר מותקן פייתון במחשב שלנו, ניתן להריץ את הפקודה `python --version` בטרמינל. במידה ואינך משתמש בגרסה האחרונה של פייתון, ניתן להתקין אותה על ידי הורדת התוכנה מהאתר הרשמי והפעלת המתקן.

פעולה הראשונה לפרסום HTML היא ליצור חיבור לאתר על מנת לקבל את הדף המבוקש. בפייתון, ניתן לעשות זאת באמצעות הפונקציה `requests.get()` כדי לבצע בקשת GET לקניה את הדף המבוקש. לדוגמה:

```Python
import requests

response = requests.get('https://www.example.com')
```

תמונה שבשורה השנייה, שקיבלנו מספר תגובה (200) המעיד על הצלחת הבקשה וקבלת הדף. כעת, ניתן להשתמש בספרייה מובנית שנקראת `BeautifulSoup` לפירסום HTML.

```Python
from bs4 import BeautifulSoup

# יצירת מופע חדש של BeautifulSodp
soup = BeautifulSoup(response.text, 'html.parser')

# עתידה לטפל בתיוגים אלה ולהחזיר את המידע הרצוי
soup.title
soup.find_all('a')
```

כפי שניתן לראות בדוגמה, ניתן להשתמש בפונקציות כמו `title` ו-`find_all` כדי להשיג מידע מהדף המבוקש.

## דיב דייב

הפירסום של HTML הוא נושא עמוק ומורכב, ולכן חשוב ללמוד כיצד ל