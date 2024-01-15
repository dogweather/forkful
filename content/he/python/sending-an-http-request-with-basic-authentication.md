---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Python: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

ברוב פעמים כאשר אנו משתמשים באפליקציות או באתרים באינטרנט, יש לנו צורך להתחבר אליהם על מנת לקבל גישה לתכנים מגוונים. הכניסה למערכת מוגבלת במידת הצורך באופן שעונה על דרישות האבטחה של האתר. במקרים רבים, אחת השיטות הנפוצות להתחברות הנקראת "אימות בסיסי", המשתמשים כינים זאת בכינוי "אימות מפתח API" או "אימות מפתח HTTP" על מנת לתת המלצה על מימוש.

## איך לבצע

כדי להראות כיצד ניתן לבצע בקשת HTTP עם אימות בסיסי בשפת פייתון, נשתמש בספריית "requests". כדי להשתמש באימות בסיסי, נצטרך להעביר את שם המשתמש והסיסמה לפונקציה "auth" כתכנית ראשית.

```python
import requests

# יצירת משתנה עם שם המשתמש והסיסמה
auth = ("username", "password")

# שליחת בקשת GET עם אימות בסיסי לאתר כלשהו
response = requests.get("https://example.com", auth=auth)

# הדפסת קוד המצב של התגובה
print(response.status_code)

# יצירת משתנה עם נתוני אימות
data = {"username": "username", 
        "password": "password"}

# שליחת בקשת POST עם אימות בסיסי לאתר כלשהו
response = requests.post("https://example.com/login", data=data)

# הדפסת תוכן התגובה
print(response.text)
```

כפי שניתן לראות בדוגמה הקודמת, אפשר לשלוח בקשות GET או POST עם אימות בסיסי, תוך שימוש בפרמטר "auth" לפונקציות של "requests".

## Deep Dive

כאשר שולחים בקשה עם אימות בסיסי, הנתונים מועברים כטכסט רגיל על ידי בקשה הכוללת את שם המשתמש והסיסמה בפורמט של "שם משתמש:סיסמה". המידע הזה מ