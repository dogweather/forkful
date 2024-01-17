---
title:                "שליחת בקשת http"
html_title:           "Python: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP היא פעולה חשובה בעולם התכנות, המאפשרת לנו לפנות לשרת ולבקש ממנו לחזור לנו מידע מסוים. תוכנתנים משתמשים בזה כדי לקבל מידע מאתרים שונים ולפנות אליהם עם נתונים כדי לבצע פעולות שונות.

## איך לעשות זאת:

השיטה הכי מקובלת לשליחת בקשת HTTP בפייתון היא להשתמש בספריית `requests`. ניתן להתחיל בהתקנת הספרייה באמצעות הפקודה ```pip install requests``` ולאחר מכן לייבא אותה בתוך הקוד שלנו עם הפקודה `import requests`. לאחר מכן, ניתן לעשות בקשה לכתובת מסוימת באמצעות הפקודה `requests.get('url')`, ולקבל חזרה את התוכן של העמוד בפורמט של `Response` object.

## שירות עמוק:

שליחת בקשת HTTP היא תהליך שכבר קיים מזמן רב והוא משמש כמעט בכל אתר אינטרנט. בעזרת זה, תוכלו לפנות לשרתים שונים ולקבל מידע בצורה פשוטה וקלה. חלק מהאלטרנטיבות לשימוש בספריית `requests` הן `urllib` ו-`httplib`, אך `requests` מציעה תמיכה בפונקציונליות נוספת ונוחות יותר למשתמש.

## ראו גם:

- [תיעוד רשמי של הספרייה requests] (https://requests.readthedocs.io/en/master/)
- [מדריכים וערוצים לימוד של תיכנות בפייתון] (https://www.learnpython.org/en/)