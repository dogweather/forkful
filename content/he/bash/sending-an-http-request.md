---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

אף אחד לא אוהב לגועל כאשר האתר שלו לא "עובד"! השליחה של בקשות HTTP זהו כלי חיוני שיכול לסייע לנו לבדוק את האתר שלנו ולאתר את הבעיות שיכולות להתחזק בו. למזלנו, Bash מציעה דרך פשוטה ויעילה לשלוח בקשות HTTP ישירות מהצד שלנו.

## איך לעשות

בדוגמאות הקוד הבאות, נראה כיצד לשלוח בקשות GET, POST ו-PUT באמצעות Bash וכיצד לקרוא את התגובות שנקבל. תהנו!

```Bash
# שליחת בקשת GET
curl https://www.example.com

# שליחת בקשת POST עם נתונים נוספים (Headers, Body)
curl -X POST https://www.example.com -H 'Content-Type: application/json' -d '{"name": "John", "age": 30}'

# שליחת בקשת PUT עם נתונים כתובים מראש
curl -X PUT https://www.example.com -d 'New content'

# קריאת תוצאות
# 200 - כל הכבוד! הבקשה הושלמה בהצלחה!
# 400 - משהו לא בסדר עם הבקשה שנשלחה
# 401 - אין לך הרשאה לבצע את הבקשה
# 404 - האתר לא נמצא
# 500 - בעיה בצד שרת, נסה שוב מאוחר יותר
```

## חקירה מקיפה

כדי לשלוח בקשות HTTP מתוך Bash, אנו משתמשים בכלי נקרא "curl". זהו כלי פופולרי ועמיד, המאפשר לנו לשלוח בקשות מצד לקוח ולקבל מענה מהשרת. עם ניצול הטכנולוגיות הזמינות ב-Bash (כמו משתנים ולולאות), אנו יכולים לבנות סקריפטים מוטיבציוניים המשתמשים ביכולות של Curl לפילוח וניתוח מידע מצורף.

## ראה גם

למד עוד על כלי ה-Curl ואיך להשתמש בו בכתיבת סקריפטים במדריך הבא: https://www.youtube.com/watch?v=7Wklo2qotEU

קבל תמונה מכיוון שלפניה אתה יכול להשתמש ב