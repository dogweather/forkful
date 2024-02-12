---
title:                "פיענוח HTML"
aliases: - /he/haskell/parsing-html.md
date:                  2024-02-03T19:12:42.974817-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח HTML בHaskell מאפשר לך לחלץ נתונים, לשנות תוכן HTML, או להתקשר עם דפי אינטרנט תכנותית. פעולה זו הכרחית למשימות כמו גריפת אתרים, בדיקות אוטומטיות של אפליקציות אינטרנט, וחילוץ נתונים מאתרים - תוך ניצול המערכת הטיפוסית החזקה של Haskell ופרדיגמות התכנות הפונקציונליות כדי להבטיח קוד חזק ותמציתי.

## איך:

לצורך פיענוח HTML בHaskell, נשתמש בספרייה `tagsoup` בשל פשטותה וגמישותה. ראשית, וודאו שהספרייה מותקנת על ידי הוספת `tagsoup` לקובץ הcabal של הפרויקט שלכם או על ידי הרצת `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- HTML לדוגמה להדגמה
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>Click Here</a></body></html>"

-- לפענח HTML ולסנן לינקים (תגי a)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- להדפיס קישורים שנחלצו
print links
```

פלט לדוגמה:
```plaintext
["http://example.com"]
```

לצרכי פיענוח HTML מורכבים יותר, שקלו להשתמש בספריית `pandoc`, במיוחד אם אתם עוסקים בהמרת מסמכים. היא גמישה ביותר אך מגיעה עם מורכבות רבה יותר:

```haskell
import Text.Pandoc

-- בהנחה שיש לכם מסמך Pandoc (doc) טעון, לדוגמה, מקריאת קובץ
let doc = ... -- המסמך Pandoc שלכם יופיע כאן

-- להמיר את המסמך למחרוזת HTML
let htmlString = writeHtmlString def doc

-- כעת, יש לפענח את `htmlString` כמתואר לעיל או להמשיך לפי הצורך שלכם.
```
זכרו כי `pandoc` היא ספרייה גדולה המתמקדת בהמרה בין פורמטים רבים של סימון, אז השתמשו בה אם אתם זקוקים ליכולות הנוספות שלה או אם אתם כבר מתמודדים עם פורמטים של מסמכים ביישום שלכם.
