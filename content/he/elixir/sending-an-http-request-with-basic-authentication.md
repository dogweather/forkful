---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Elixir: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא פעולה בה תוכנתאים משתמשים כדי לקבל גישה למידע משרתי אינטרנט מאובטחים. תוכניות כמו תוכנות מחשב או אפליקציות ניידות צריכות לשלוח בקשות כאלה כדי לקבל גישה לפרטי משתמש תוך הבטחת פרטיות ואבטחת מידע.

## איך לעשות זאת:

```Elixir
# דוגמא של שליחת בקשת HTTP עם אימות בסיסי באמצעות הספרייה HTTPoison
response = HTTPoison.get("https://example.com/resource", headers: [
  {"Authorization", "Basic YWRtaW46cGFzc3dvcmQ="} # הכנס כאן את שם המשתמש והסיסמה שלך בקידוד base64
])

# הדפסת קוד תגובה
IO.puts(response.status_code)

# הדפסת גוף תגובה
IO.puts(response.body)
```

## חקירה מעמיקה:

מאז הומצאה פרוטוקול ה-HTTP בשנות השישים של המאה העשרים ועד היום, פרוטוקול זה הפך לסטנדרט בעולם האינטרנט. כך שלשליחת בקשת HTTP עם אימות בסיסי יש מספר שיטות נוספות כמו OAuth או Token מסוג JSON Web.

פרוטוקול אימות בסיסי טבעו מבוסס טכנולוגיית הטקסט הבתולי ומכן הוא מיועד רק לפראנסים שמעדיפים שימוש פשוט ויעיל. תיקנוים כמו YARN ו NPM מרחיבים את האופקים עם שליחת הבקשות האלה תוך התאמה לבקשות ב API שונות.

## ראה גם:

- [HTTPoison - מסמך API פלאטפורמת Elixir](https://hexdocs.pm/httpoison/1.6.1/HTTPoison.html)
- [OAuth - מה זה ואיך זה עובד?](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2-hebrew)
- [JSON Web Token - הוראות לשימוש ב Elixir עבור בקשות HTTP בסיסיות](https://hexdocs.pm/joken/2.0.5/Joken.html)