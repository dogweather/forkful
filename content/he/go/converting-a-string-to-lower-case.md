---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת מחרוזת לאותיות קטנות היא פונקצייה שהופכת את כל האותיות במחרוזת לאותיות קטנות. תכנתים משתמשים בזה במקרים בהם לא רלוונטי או רצוי להבדיל בין אותיות גדולות לקטנות - למשל, בדיקות שוויון, מיון, חיפוש ועוד.

## איך עושים את זה?
ראשית, ניבא את הספרייה המתאימה:

```Go
import "strings"
```

בהנחה שיש לנו מחרוזת, ממרים אותה לאותיות קטנות כך:

```Go
original := "Hello, World!"
lower := strings.ToLower(original)
```

הפלט של הקוד הזה יהיה: 
```Go
"hello, world!"
```

## צוללים עמק
בעבר, המרת מחרוזת לאותיות קטנות הייתה תהליך מורכב יותר שכלל המרת כל אות לאות קטנה בנפרד. עם השנים, זה השתפר הרבה והיום אנו משתמשים בפונקציה מובנית של השפה כדי להפוך את המחרוזת שלנו לאותיות קטנות. יש גם שיטות חלופיות, כמו לולאות, אך הן לא יעילות כמו השיטה המובנית.

## להמשיך לקרוא
1. [מסמך הפונקציות הרשמי של Go](https://golang.org/pkg/strings/#ToLower)
2. [מדריך לעבודה עם מחרוזות ב-Go](https://www.callicoder.com/golang-string/)
3. [איך לשלוט במחרוזות: מדריך של Go](http://www.golangpro.com/2016/03/manipulate-string-golang.html)