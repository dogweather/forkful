---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- /he/fish-shell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:15.829235-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי משמשת לאימות של משתמשים כאשר מתבצע גישה למשאב המוגן באינטרנט. מתכנתים משתמשים בתהליך זה כאשר רוצים לוודא שהמשתמש אשר מבקש להתחבר הוא אכן מורשה לקבל את המידע או לבצע פעולה מסוימת בשרת.

## איך לעשות:
```Fish Shell
# הגדרת שם המשתמש והסיסמה
set user 'shani'
set password 's3kr3t'

# יצירת ערך מקודד באימות בסיסי
set auth (echo -n "$user:$password" | base64)

# שליחת בקשת GET עם אימות בסיסי
curl -H "Authorization: Basic $auth" https://example.com/resource
```
דוגמא לפלט:
```
{
  "data": "מידע סודי שרק משתמשים מורשים יכולים לראות"
}
```

## צלילה לעומק
במובן ההיסטורי, אימות בסיסי ב-HTTP הוא אחד משיטות האימות הראשונות שהושם בשימוש. למרות שהוא נחשב לפחות בטוח יותר משיטות אימות מודרניות כמו OAuth, הוא עדיין בשימוש משום שהוא פשוט ליישום ויש תמיכה נרחבת בלקוחות HTTP שונים. חשוב לדעת שבקשות עם אימות בסיסי צריכות להתבצע תמיד דרך HTTPS כדי למנוע חשיפה של נתוני האימות. קיימות אלטרנטיבות כמו tokens, אימות דו-שלבי ו-API keys שמספקות בטחון גבוה יותר, אך לעיתים עדיין משתמשים באימות בסיסי לפעולות פשוטות או כאשר רמת הבטחון הנדרשת אינה גבוהה.

## ראה גם
- מדריך לפקודת `curl`: https://curl.se/docs/manpage.html
- מידע על אימות בסיסי ב-HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- מידע על תקן האבטחה HTTPS: https://en.wikipedia.org/wiki/HTTPS
