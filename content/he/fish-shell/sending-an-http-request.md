---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא התנהלות מרכזית בעבודת המפתחים של היום, מאפשרת תקשורת ביננו לבין השרת. מתכנתים בוצעים שליחת בקשת HTTP כדי לקבל מידע, לעדכן, למחוק או ליצור משאבים בשרת.

## איך עושים?
כאן יש דוגמה לאיך משלחים בקשת HTTP מתוך סקריפט Fish Shell:

```fish

set url 'http://mockbin.org/bin/9b9a2279-1e6f-4df3-875c-d9b0bc1a5378/view'
set headers 'Content-Type' 'application/json'
set body '{"key":"value"}'

curl $url -X POST -H $headers -d $body
```
זה יציג לך את התוצאה שהוחזרה מהשרת.

## בהקשר הרחב
Fish Shell הוא שפת תסריטים מחשב מודרנית שמשלבת את היכולת לשלוח בקשות HTTP בצורה פשוטה ויעילה. חלופה לשליחת בקשות HTTP לפי Fish Shell היא באמצעות שפות תכנות אחרות כמו Python או Javascript. ברמת המימוש, בשימוש הפקודה 'curl' שהיא כלי שפועל בשורת הפקודה של מערכות ההפעלה וביצוע בקשת HTTP.

## ראו גם
עיין במקורות הבאים למידע מורחב לגבי שליחת בקשות HTTP באמצעות Fish Shell:

- דף הבית של Fish Shell: https://fishshell.com
- עזרה בנושא 'curl': https://curl.haxx.se/
- מדריך לשפת תכנות Fish Shell: https://fishshell.com/docs/current/index.html