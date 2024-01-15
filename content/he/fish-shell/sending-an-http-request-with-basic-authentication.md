---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Fish Shell: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה 
כדי לשלוח בקשת HTTP עם אימות בסיסי ולקבל גישה למידע מאתר אינטרנט מוגן. זה מאפשר גישה מאובטחת למידע רגיש ומגביר את האבטחה של האתר.

# איך לעשות 
עליכם להתקין את עט השבבים של Fish Shell ולהשתמש בו לשם כתיבת קוד שילוב אימות בסיסי בבקשות HTTP. להלן דוגמאות לכתיבת קוד ולקבלת תוצאות בפורמט של "```Fish Shell ... ```". 

קוד להתחברות לאתר שצורף אימות בסיסי:
```
fish -c "curl -u username:password example.com"
```

כתיבת בקשת GET עם אימות בסיסי והדפסת התוכן של התגובה:
```
fish -c "curl -u username:password example.com/api/data | jq .content"
```

# חפירה עמוקה 
כאשר משתמשים באימות בסיסי בבקשות HTTP, ניתן להשתמש באפשרויות נוספות כגון הפעלת פקודת CURL ברקע, שילוב עם טוקן אבטחה, ובחירת אימות מתאים עבור האתר המטרה. עם התרחבות נוספת של פקודות כגון HTTPie וטוקן-שוקלט הוספת אימות בסיסי נעשה בעזרת פקודות קצרות יותר וקריאות יותר.

# ראה גם 
- [מדריך רשמי על פקודת CURL באתר הנתמך על ידי Fish Shell](https://fishshell.com/docs/current/commands.html)
- [התקנת Fish Shell](https://fishshell.com/docs/current/installing.html)
- [HTTP באתר הוויקי של Fish Shell](https://fishshell.com/docs/current/commands.httpie.html)