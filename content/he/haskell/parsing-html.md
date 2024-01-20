---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסום HTML הוא התהליך שבו מנתחים את מבנה דף האינטרנט לשפת התכנות שלנו. מתכנתים עושים את זה כדי לאפשר אינטראקציה ברובוסטית יותר עם אתרים ונתונים.

## איך לעשות:
הפונקציה `parseTagText` מנתחת טקסט לטגים:

```Haskell
import Text.HTML.TagSoup

parseTagText :: String -> [Tag String]
parseTagText = parseTags
```

לדוגמה:

```Haskell
parseTagText "<html><body>Hello, World!</body></html>"
```

שיצא:

```Haskell
[TagOpen "html" [], TagOpen "body" [], TagText "Hello, World!", TagClose "body", TagClose "html"]
```

## צלילה עמוקה
1. **הקשר ההיסטורי**: פירסום HTML הוא חלק מהסטנדרט XML שפותח בשנים המאוחרות של שנות ה-90. 
2. **חלופות**: ישנן ספריות אחרות להאסל שיכולות לנתח HTML, כולל `hxt` ו-`tagsoup`.
3. **פרטי יישום**: `parseTags` משתמשת במנגנון `TagSoup` לטיפול ב-HTML שגוי.

## ראו גם
- מדריכים אחרים:
   - הגידה ל- `TagSoup` ([לינק](https://hackage.haskell.org/package/tagsoup-0.14.8/docs/Text-HTML-TagSoup.html)).
   - כתיבת HTML Parser משלך ([לינק](https://www.fpcomplete.com/haskell/tutorial/parsing-html/)).
- ספריות ב- Haskell לניתוח HTML: `hxt`, `tagsoup`, שמיישמות שיטות שונות.