---
date: 2024-01-26 04:29:29.941811-07:00
description: "XML \u05D4\u05D5\u05D0 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  \ \u05DC\u05E7\u05D9\u05D3\u05D5\u05D3 \u05DE\u05E1\u05DE\u05DB\u05D9\u05DD \u05D1\
  \u05D3\u05E8\u05DA \u05E9\u05D4\u05D9\u05D0 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D2\
  \u05DD \u05DC\u05D0\u05D3\u05DD \u05D5\u05D2\u05DD \u05DC\u05DE\u05DB\u05D5\u05E0\
  \u05D4. \u05D4\u05D9\u05D0 \u05DE\u05E4\u05EA\u05D7\u05D9\u05EA \u05D1\u05E9\u05D9\
  \u05E8\u05D5\u05EA\u05D9 \u05E8\u05E9\u05EA, \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\
  \u05D5\u05E8\u05D4, \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05DE\
  \u05E2\u05D1\u05D9\u05E8\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\
  \u05D5\u05E8\u05DE\u05D8\u2026"
lastmod: '2024-03-11T00:14:12.165248-06:00'
model: gpt-4-0125-preview
summary: "XML \u05D4\u05D5\u05D0 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\u05D5\u05DF\
  \ \u05DC\u05E7\u05D9\u05D3\u05D5\u05D3 \u05DE\u05E1\u05DE\u05DB\u05D9\u05DD \u05D1\
  \u05D3\u05E8\u05DA \u05E9\u05D4\u05D9\u05D0 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D2\
  \u05DD \u05DC\u05D0\u05D3\u05DD \u05D5\u05D2\u05DD \u05DC\u05DE\u05DB\u05D5\u05E0\
  \u05D4. \u05D4\u05D9\u05D0 \u05DE\u05E4\u05EA\u05D7\u05D9\u05EA \u05D1\u05E9\u05D9\
  \u05E8\u05D5\u05EA\u05D9 \u05E8\u05E9\u05EA, \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\
  \u05D5\u05E8\u05D4, \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05DE\
  \u05E2\u05D1\u05D9\u05E8\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\
  \u05D5\u05E8\u05DE\u05D8\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?
XML הוא שפת סימון לקידוד מסמכים בדרך שהיא קריאה גם לאדם וגם למכונה. היא מפתחית בשירותי רשת, קבצי תצורה, והחלפת נתונים מכיוון שהיא מעבירה נתונים בפורמט מובנה, היררכי.

## איך ל:
Clojure מציעה את הספריה `clojure.data.xml` לניתוח והפקת XML. ראשית, בואו ננתח קצת XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; ניתוח מחרוזת XML
  (println parsed))
```
פלט:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

להפיק XML ממבנים של Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
פלט:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## טבילה עמוקה
XML היה כאן עוד מאז שנות ה-90 המאוחרות, התחיל כתת-קבוצה מופשטת של SGML, במטרה להיות מיועד לנתוני אינטרנט. הוא התפוצץ בשימוש בטכנולוגיות כמו SOAP ו-XHTML אך קיבל קצת תחרות מ-JSON, שהוא מועדף בגלל הקלות והפשטות שלו.

הגישה של Clojure ל-XMl שומרת על זה פונקציונלי וממוקד נתונים, נאמן לאתוס השפה. `clojure.data.xml` היא רק אופציה אחת; יש לכם גם את `clojure.xml` לצרכים בסיסיים, ולשם התממשקות עם Java, אתם יכולים להשתמש בגדולים כמו JAXB או DOM4J.

שימו לב, הביצועים והנטל על הזיכרון כאשר מתמודדים עם מסמכי XML גדולים מאוד יכולים להיות כבדים. מנתחי זרם כמו StAX יכולים לעזור, אך תצטרכו לצאת לעולם של Java בשבילם.

## ראו גם
- [clojure.data.xml ב-GitHub](https://github.com/clojure/data.xml)
- [Java API לעיבוד XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
