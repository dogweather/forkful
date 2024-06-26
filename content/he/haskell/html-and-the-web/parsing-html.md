---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:42.974817-07:00
description: "\u05D0\u05D9\u05DA: \u05DC\u05E6\u05D5\u05E8\u05DA \u05E4\u05D9\u05E2\
  \u05E0\u05D5\u05D7 HTML \u05D1Haskell, \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 `tagsoup` \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\u05D5\
  \u05EA\u05D4 \u05D5\u05D2\u05DE\u05D9\u05E9\u05D5\u05EA\u05D4. \u05E8\u05D0\u05E9\
  \u05D9\u05EA, \u05D5\u05D5\u05D3\u05D0\u05D5 \u05E9\u05D4\u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05DE\u05D5\u05EA\u05E7\u05E0\u05EA \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D4\u05D5\u05E1\u05E4\u05EA `tagsoup` \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D4\
  cabal \u05E9\u05DC \u05D4\u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.409904-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E6\u05D5\u05E8\u05DA \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML\
  \ \u05D1Haskell, \u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 `tagsoup` \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\u05D5\u05EA\u05D4 \u05D5\u05D2\
  \u05DE\u05D9\u05E9\u05D5\u05EA\u05D4."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

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
