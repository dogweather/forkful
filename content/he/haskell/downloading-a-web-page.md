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

## למה

להזרים אתרי אינטרנט ולשכפל את המידע המופיע בהם נחשב כפעילות חשובה בתחום התכנות. ניתן לעשות זאת למטרות שונות, כגון סקירת נתונים או בניית אפליקציות ווב.

## איך לעשות זאת

אם אתם מעוניינים להזרים את אתר האינטרנט שלכם ולשכפל את המידע ממנו, אתם יכולים לעשות זאת באמצעות שפת התכנות Haskell. כדי לבצע פעולה זו, נצטרך להשתמש בספריית חיצונית בשם "HTTP-conduit", שתעזור לנו להתחבר לאתר האינטרנט ולזרום את המידע.

נתחיל עם היבוא של הספרייה המתאימה:

```Haskell
import Network.HTTP.Simple
```

כעת נשתמש בפונקציה "httpSource", שתקבל כפרמטר את הכתובת של האתר המטרה. נשתמש גם בפונקציה "getResponseBody" כדי לקבל את המידע הרלוונטי מהאתר.

```Haskell
res <- httpSource "https://www.example.com"
let body = getResponseBody res
```

נכון לעכשיו, כל המידע מהאתר נמצא במשתנה שמכיל את השורה "getResponseBody res". אפשר להדפיס אותו באמצעות "putStrLn" כדי לראות את התוצאה המלאה.

## טיפול עמוק

כדי לשכפל את המידע מהאתר כמו שצריך, נוכל להשתמש בפונקציות נוספות כמו "parseRequest", "executeRequest" ו-"getResponseBody". עם כל אחת מהן נוכל לעבוד עם תכולת האתר ולעשות את הטיפולים הדרושים לנו. כמו כן, ניתן לכתוב תכניות שיכולות להזרים אתרי אינטרנט באופן אוטומטי על בסיס יומי, שבועי או חודשי.

## ראו גם

- [דוגמת קוד ב-Haskell להזרמת אתר אינטרנט](https://gist.github.com/ChemiHat/a479df611921063ff6c