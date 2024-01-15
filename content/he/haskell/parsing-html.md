---
title:                "פילוח html"
html_title:           "Haskell: פילוח html"
simple_title:         "פילוח html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## למה

הפארסינג של קוד HTML הינו כלי שימושי ביותר בתחום התכנות ובפיתוח אתרים. הפונקציות המתאימות לפארסינג מאפשרות לנו לקרוא ולעבד נתונים מקוד HTML בצורה יעילה ומהירה, מה שנותן לנו שליטה משולבת על איך הנתונים מוצגים ומשמשים באתר שלנו.

## איך להשתמש

הנה דוגמאות של קוד HTML בשפת הסקל שימושיות לפארסינג:

```Haskell
-- טעינת ספריות הפארסינג של HTML

import Text.HTML.DOM (parseDOM)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, descendant, ($//), (&|), (&//), (>=>))
import qualified Data.Text as T

-- יצירת משתנה עם קוד HTML

html :: T.Text
html = "<html><body><h1>Hello World</h1></body></html>"

-- משתנה של cursor עם אתחול ריק

cursor :: Cursor
cursor = fromDocument (parseDOM html)

-- חיפוש לפי אלמנטים מסוימים בקוד

cursor $// element "h1" &// content

-- Output: ["Hello World"]

-- חיפוש לפי תכונת מסוימת בקוד כמו "class" או "id"

cursor $// element "div" >=> attributeIs "class" "container" &// content

-- Output: ["Content of the div with class 'container'"]
```

## רקע עמוק

כאשר אנו פורסים קוד HTML, אנו ממירים את הקוד למבנה נתונים שנוכל לעבוד איתו בקלות וכולל כמה פונקציות מועילות כגון חיפוש לפי אלמנטי HTML מסוימים או עדכון נתונים בעת יצירת אתר דינמי. הספריות המובנות בשפת הסקל כוללות כמה קונסטרוקטורים ופונקציות שנועדו ללכוד את הקוד HTML ולייצר ממנו מבנה נתונים שניתן לעבוד איתו.

## ראה גם

- [מדריך לספריות הפארסינג של HTML בשפת הסקל](https://hackage.haskell.org/package/tagsoup)
- [חיפוש מתקדם בקוד HTML בשפת הסקל](https://hackage.haskell.org/package/html-conduit)