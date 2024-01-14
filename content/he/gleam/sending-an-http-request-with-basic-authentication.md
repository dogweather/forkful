---
title:                "Gleam: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה?
מידע כללי על מה למעשה הוא "הכנסה למערכת" - שורה אחת הסבר על *למה* מישהו ירצה לשלוח בקשת HTTP עם אימות בסיסי.

## איך לעשות?
נתחיל עם קצת קוד כדי להסביר איך לממש בקשת HTTP עם אימות בסיסי ב-Gleam. תוכלו לראות את הקוד המלא ואת הפלט המתואר בדוגמא הבאה:

```Gleam
import gleam/http

let credentials = http.basic_auth("username", "password")
let url = "https://example.com/api"
let headers = [http.header("Content-Type", "application/json")]
let body = """{"key": "value"}"""
let response = http.post(url, http.request(headers, Some(credentials), body))
http.response_status(response) // 200
```

התוכנית תשלח בקשת POST ל-https://example.com/api עם הכותרת "Content-Type" וגוף מסוג JSON, ותשתמש במידע שסופק במשתנה credentials כדי לבצע אימות בסיסי. דוגמאות נוספות ודרך מלאה ליישום המנגנון הזה ניתן לראות בתיעוד הרשמי של Gleam.

## צלילה עמוקה
כעת שהתחלנו להתוודע לכיצד לשלוח בקשת HTTP עם אימות בסיסי ב-Gleam, בואו נרחיב על כמה נקודות שאולי לא ברורות ממקום שכזה:

- בדוגמא שהצגנו למעלה, הוכתר בקשת ה-post עם הכותרת "Content-Type" כדי לציין לשרת את סוג התוכן שנשלח בגוף הבקשה. למרבה המזל, מרבית הספקים יחזירו את שגיאת ה-Hardcode במקרה של פניית GET לקובץ JSON, אבל מומלץ לכתוב את הכותרת הזו בכל זאת.
- כדי לבצע אימות בסיסי, ניתן להשתמש בפונקציה http.basic_auth הזו מחזירה את הנתונים שסופקו כפרמטרים. אם אימות אינו נדרש, אפשר לעבור את ערך `None` במקום כמו בדוגמא.
- ניתן גם להשתמש בפונקציות האחרות הניתנות לזיהוי אימות, כמו `http