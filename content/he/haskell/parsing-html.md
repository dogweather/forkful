---
title:                "ניתוח HTML"
date:                  2024-01-20T15:32:57.537478-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסום HTML הוא התהליך שבו תוכנה מפרקת את תוכן דף אינטרנט מפורמט HTML למרכיבים שפת תכנות יכולה לעבוד איתם. תכניתנים עושים זאת כדי לאסוף נתונים, לבצע בדיקות אוטומטיות, או לשנות תוכן בדפים.

## איך לעשות:
בואו נבחן את הספרייה `tagsoup` הנפוצה ב-Haskell לפירסום HTML.
```Haskell
import Text.HTML.TagSoup

-- דוגמא פשוטה לפירוס HTML
parseHTML :: String -> [Tag String]
parseHTML html = parseTags html

-- נניח שיש לנו את ה-HTML הבא:
exampleHTML :: String
exampleHTML = "<html><head><title>דוגמא</title></head><body><p>זו דוגמא לפסקה מפורמטת ב-HTML.</p></body></html>"

-- הפעלה:
main :: IO ()
main = print $ parseHTML exampleHTML

{- פלט לדוגמא:
[TagOpen "html" [],TagOpen "head" [],TagOpen "title" [],TagText "דוגמא",TagClose "title",TagClose "head",TagOpen "body" [],TagOpen "p" [],TagText "זו דוגמא לפסקה מפורמטת ב-HTML.",TagClose "p",TagClose "body",TagClose "html"]
-}
```
עם זאת, ברוב המקרים נרצה למצוא תגים מסוימים ולחלץ את התוכן שלהם:
```Haskell
import Text.HTML.TagSoup

-- חיפוש תגית כותרת וחילוץ הטקסט
findTitle :: [Tag String] -> String
findTitle = innerText . takeWhile (~/= "</title>") . dropWhile (~/= "<title>")

main :: IO ()
main = print $ findTitle $ parseHTML exampleHTML

-- פלט: "דוגמא"
```
## צלילה עמוקה
הספרייה `tagsoup` הוצגה לראשונה בשנת 2006 והיא מתמקדת בגמישות וחסינות לשגיאות, המאפשרת עיבוד HTML "בעולם האמיתי" גם אם הוא לא תקני לחלוטין. קיימות גם ספריות אלטרנטיביות, כגון `hxt` המאפשרת עבודה עם XPath ו-XSLT, ו-`pandoc` לתכניתנים שמעוניינים במרחב רחב יותר של פירמטים ותכנים. `tagsoup` משתמשת בכללי העיסוק של "אם זה נראה כמו HTML, זה כנראה HTML", ובכך מנצחת רוב המקרים של הפרדה שגויה או שימוש לא תקני בתגי HTML.

## ראו גם
- הדוקומנטציה הרשמית של [`tagsoup`](https://hackage.haskell.org/package/tagsoup).
- ספריית [`hxt`](https://hackage.haskell.org/package/hxt), אלטרנטיבה לפירסום ועיבוד XML ו-HTML. 
- כלי [`pandoc`](https://pandoc.org/), להמרות מתוך ולתוך מגוון פורמטים שונים של מסמכים.