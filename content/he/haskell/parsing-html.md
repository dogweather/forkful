---
title:                "Haskell: פירוק HTML"
simple_title:         "פירוק HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## מדוע

זיהוי ופתרון בעיות בתכנות הוא אחד המשימות המרתקות והאתגריות ביותר בעולם התכנות. אחת מהסיבות העיקריות לביצוע זיהוי משחקי HTML היא היכולת לקרוא ולהבין כל מיני חלקי HTML ואתרים חיצוניים שקיימים בתוך אתר האינטרנט שלנו.

## איך לעשות זאת

```Haskell
import Text.HTML.TagSoup

-- כדי להאיצג דף HTML בתכנית ההזיהוי שלנו, נאכיל את הדף לפונקציית parseTags ונשתמש בפונקציות filter ונהג "ימין"
main :: IO ()
main = do
    html <- readFile "index.html"
    let tags = parseTags html
        links = filter (isTagOpenName "a") tags
        names = map (fromAttrib "title") links
    print names
```

כמו שאתה יכול לראות בקוד הזה, אנו משתמשים בספריית Text.HTML.TagSoup על מנת לזהות את התגיות השונות בדף הHTML. במשך תהליך הנתונים, אנו משתמשים בפונקציות שונות כגון filter וmap בכדי לנתח ולקרוא את הנתונים הרלוונטיים.

## חקירה מעמיקה

לאחר שלמדנו איך להזיהוי ולקרוא נתונים בHTML במשך תהליך התכנות, ניתן לחקור עוד על ידי התמקדות בפתרונות וכלים נוספים שניתן להשתמש בהם. כמו כן, ניתן ללמוד עוד על בעיות שונות שניתן לפתור באמצעות זיהוי HTML ועל הדרכים האפשריות להיווצרות תקלות ואיך לתקן אותם.

## ראה גם

- [ספריית Text.HTML.TagSoup](https://hackage.haskell.org/package/tagsoup)
- [מדריך להיכרות עם זיהוי HTML ב־Haskell](https://www.schoolofhaskell.com/user/commercial/content/scraping-and-parsing-html)