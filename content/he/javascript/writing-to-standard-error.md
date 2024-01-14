---
title:    "Javascript: כתיבה לתקן השגיאה הסטנדרטי"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

כתיבה לסטנדרט השגיאה ב-Javascript

## למה

לכתוב לסטנדרט השגיאה ב-Javascript הוא מנגנון שימושי ביותר עבור מפתחים. הוא מאפשר לנו להדפיס הודעות שגיאה ישירות לטרמינל, מאפשר לנו לאתר בקלות את התקלות ולנתח ולתקן אותן.

## כיצד להשתמש

ראשית, נצרף למסמך את פונקציית "console", המיועדת עבור פלט לטרמינל. נשתמש בפונקציית "error" על מנת להדפיס לסטנדרט השגיאה. לדוגמה:

```Javascript
console.error("זהו הודעת שגיאה");
```

פלט בטרמינל יהיה:

```
זהו הודעת שגיאה
```

ניתן להשתמש גם במתודת "process.stderr.write()" על מנת להדפיס מחרוזת או משתנה לתוך סטנדרט השגיאה. לדוגמה:

```Javascript
process.stderr.write("אני עובד עם המשתנה " + myVariable);
```

הפלט בטרמינל יהיה:

```
אני עובד עם המשתנה value
```

## נכנסים לעומק

הפעלת פונקציית "console.error()" מפעילה את פונקציית "util.format()" בצורה אוטומטית, המאפשרת לנו להעביר מספר ארגומנטים ולהדפיס כל משתנה בנפרד. לדוגמה:

```Javascript
console.error("אני עובד עם המשתנים: %s, %s", myVariable1, myVariable2);
```

הפלט בטרמינל יהיה:

```
אני עובד עם המשתנים: ערך 1, ערך 2
```

## ראיה נוספת

- מדריך על כתיבה לאירורל של השגיאות [A Guide to Writing Error Prone JavaScript](https://www.exceptionnotfound.net/a-guide-to-writing-error-prone-javascript/)
- מידע נוסף על פונקציית "console.error()" [Console - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)