---
title:                "הורדת עמוד אינטרנט"
html_title:           "Haskell: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא פעולה פשוטה בה נטען דף אינטרנט מהאינטרנט ונשמר כקובץ על המחשב. הפעולה הזו עשויה להיות מועילה לפתיחת אתרים מהירה יותר או לשימוש בתוכן שנשמר באינטרנט במקום גישה רגילה לרשת.

## איך לעשות?
הנה דוגמאות לכתיבת הורדת דף אינטרנט בקוד ה-Haskell באמצעות חבילת "HTTP":
```Haskell
import Network.HTTP
main = do
  response <- Network.HTTP.simpleHTTP (getRequest "https://www.example.com")
  body <- getResponseBody response
  putStrLn body
```
פלט:
```Haskell
<html>...</html>
```
כתיבה של קובץ מסוים עם תוכן הדף המורד:
```Haskell
import Network.HTTP
main = do
  response <- Network.HTTP.simpleHTTP (getRequest "https://www.example.com")
  body <- getResponseBody response
  writeFile "page.html" body
```

## טביעה עמוקה
### היסטוריה
ההורדה של דפי אינטרנט נוצרה עם תחילת האינטרנט כדי לקלוט את התוכן הנמצא על המסך ולשמרו בקובץ, מאז נוצרו דרכים רבות לעשות זאת עם טכנולוגיות שונות כמו פרוטוקול ה-FTP.

### אלטרנטיבות
פעולת ההורדה ניתנת לביצוע גם באמצעות שפת תכנות אחרות כמו Python או Java עם השימוש בחבילות מתאימות.

### פירוט המימוש
פעולת ההורדה ניתנת לביצוע באמצעות פונקציות בסיסיות המאפשרות גישה לרשת כמו פונקציית "getRequest" ו-"simpleHTTP" המגיעות עם החבילה "HTTP".

## ראש מעמד
כדי לתרגם את הקוד לשפה אחרת או לקרוא עוד על הנושא, ניתן לבקר באתר הרשמי של החבילה "HTTP" ב-Hackage: https://hackage.haskell.org/package/HTTP