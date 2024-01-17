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

## מה זה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה שמאפשרת לנו להתחבר לשרתים שונים ולקבל מהם מידע. מתכנתים מבצעים פעולה זו כדי לגשת למידע או לשלוח מידע לשרתים כגון APIs.

## כיצד לעשות זאת:
דוגמאות לקוד ופלט דוגמה נמצאים בתוך חסרוני הקוד של "Fish Shell" (Fish Shell ```)ʼʼʼ.

שליחת בקשת GET עם אימות בסיסי:
```
curl -u user:password http://example.com/api
```
כאשר "user" הוא שם המשתמש ו-"password" הוא הסיסמה שמופרדים בסימן ":". ניתן להשתמש גם בצורה זו:
```
curl --basic -u user:password http://example.com/api
```
הכינוי "--basic" מגלה לפקודה שאנו מבצעים אימות בסיסי.

## טיול עמוק:
הקשר היסטורי, אלטרנטיבות ופרטים מבואקשרים לשליחת בקשת HTTP עם אימות בסיסי. תוכלו למצוא את כל המידע שאתם צריכים ברשת.

כדי להגן על הפרטיות ולמנוע גישה למידע ללא הרשאות, מתכנתים משתמשים במספר כלים נוספים כדי לבצע אימות בתוך בקשת HTTP. אחד הכלים הפופולריים ביותר הוא OAuth והוא מספק ערכת כלים מקיפה לבצע אימות באתרים ויישומים מגוונים.

מימוי ישיר של בקשת HTTP עם אימות בסיסי עונה למצבים מסוימים ומאפשר לנו להיישם מידע מתוך אתרים ויישומים המאפשרים אימות בסיסי.

## ראו גם:
להלן קישורים למקורות נוספים המספקים מידע נוסף על שליחת בקשת HTTP עם אימות בסיסי:

- פקודת curl לשליחת בקשת HTTP עם אימות בסיסי: https://curl.haxx.se/docs/manpage
- דף הויקי של Fish Shell שמספק מידע על שליחת בקשת HTTP עם אימות בסיסי: https://fishshell.com/docs/current/cmds/curl.html
- המאמר "HTTP Authentication" באתר Mozilla Developer Network המסביר מקור וכיצד לבצע אימות בבקשות HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication