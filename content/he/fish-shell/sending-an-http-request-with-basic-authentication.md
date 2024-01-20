---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה? 
שליחת בקשת HTTP עם אימות בסיסי היא דרך שבה תוכנה יכולה לשלוח בקשה לשרת כאשר פרטי כניסה הם שימושיים. תכנתים משתמשים בכך כדי להבטיח כי השרת מאמת את מי ששולח ומקבל את הבקשה לפני שעובד איתה.

## איך לעשות:
דוגמא לקוד ב־Fish Shell שעובר לאימות מול שרת HTTP:
```Fish Shell
# הגדרת שם המשתמש והסיסמה
set username myUsername
set password myPassword

# שליחת בקשה עם אימות בסיסי
curl -u $username:$password "http://my.servers.address"
```
אם האימות הצליח, תקבל מענה מהנתיב שביקשת. אם לא - תקבל שגיאה.

## צלילה לעומק: 
אימות בסיסי נוצר כדי להקל על תקשורת לא מאובטחת עם שרתים. זה לא הכי בטוח, אך הוא נ־שים וקל לשימוש. חלופות - אימות digest (הקשה ולא כה נפוץ), או אימות Bearer (שמשתמש בטוקנים, נפוץ יותר). דרך שליחת הבקשה היא בהחלפת שמות המשתמשים והסיסמאות לבסיס 64 והוספתם לכותרת `Authorization` של הבקשה.

## ראה גם: 
1. מדריכים לשימוש ב-Fish Shell - [קישור](https://fishshell.com/docs/current/index.html)
2. מדריך לשימוש ב-cURL - [קישור](https://curl.se/docs/manual.html)
3. מידע על אימות בסיסי ב-HTTP - [קישור](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) 
4. חלופות לאימות בסיסי - [קישור](https://oauth.net/2/bearer-tokens/), [קישור](https://tools.ietf.org/html/rfc7616)