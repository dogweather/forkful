---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:09.083259-07:00
description: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D4\u05D5\u05D0 \u05E1\u05D9\u05E0\
  \u05D5\u05DF \u05D3\u05E8\u05DA \u05DE\u05D1\u05E0\u05D4 \u05D5\u05EA\u05D5\u05DB\
  \u05DF \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5 HTML \u05DB\u05D3\u05D9 \u05DC\u05D7\
  \u05DC\u05E5 \u05DE\u05D9\u05D3\u05E2. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D2\u05E9\u05EA \u05DC\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05E9\
  \u05E0\u05D5\u05EA \u05EA\u05D5\u05DB\u05DF \u05D0\u05D5 \u05DC\u05E9\u05E8\u05D5\
  \u05D8 \u05D0\u05EA\u05E8\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.621135-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D4\u05D5\u05D0 \u05E1\u05D9\u05E0\
  \u05D5\u05DF \u05D3\u05E8\u05DA \u05DE\u05D1\u05E0\u05D4 \u05D5\u05EA\u05D5\u05DB\
  \u05DF \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5 HTML \u05DB\u05D3\u05D9 \u05DC\u05D7\
  \u05DC\u05E5 \u05DE\u05D9\u05D3\u05E2."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

## איך לעשות:
Bash אינו הבחירה הראשונה לפרסור HTML, אך ניתן לבצע זאת בעזרת כלים כמו `grep`, `awk`, `sed`, או כלים חיצוניים כמו `lynx`. למען העמידות, נשתמש בכלי `xmllint` מהחבילה `libxml2`.

```bash
# התקן את xmllint אם יש צורך
sudo apt-get install libxml2-utils

# HTML לדוגמא
cat > sample.html <<EOF
<html>
<head>
  <title>דף לדוגמא</title>
</head>
<body>
  <h1>שלום, Bash!</h1>
  <p id="myPara">Bash יכול לקרוא אותי.</p>
</body>
</html>
EOF

# ניתוח הכותרת
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "הכותרת היא: $title"

# חילוץ פסקה לפי מזהה
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "תוכן הפסקה הוא: $para"
```

פלט:
```
הכותרת היא: דף לדוגמא
תוכן הפסקה הוא: Bash יכול לקרוא אותי.
```

## צלילה עמוקה
בעבר, תוכניתנים השתמשו בכלים מבוססי ביטויים רגולריים כמו `grep` לסריקה של HTML, אך זה היה מגושם. HTML אינו רגולרי - הוא קונטקסטואלי. כלים מסורתיים מפספסים זאת ועלולים להיות תועים.

אלטרנטיבות? לרוב. Python עם Beautiful Soup, PHP עם DOMDocument, JavaScript עם פרסרים של DOM - שפות עם ספריות שתוכננו להבין את מבנה ה-HTML.

השימוש ב-`xmllint` בתסריטי bash הוא מוצק למשימות פשוטות. הוא מבין XML, ובהארכה, XHTML. HTML רגיל יכול להיות לא צפוי, עם זאת. הוא לא תמיד עוקב אחרי כללי ה-XLM המחמירים. `xmllint` כופה על HTML להתאמץ למודל של XML, דבר שעובד היטב עבור HTML מסודר היטב, אך עלול להיתקל בבעיות עם קוד לא מסודר.

## ראה גם
- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): מפשט את HTML DOM.
- [MDN Web Docs - ניתוח ושרשור XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): לעקרונות ניתוח של XML החלים גם על XHTML.
- [מסמכי Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): ספרייה של Python לניתוח HTML.
- [מסמכי libxml2](http://xmlsoft.org/): פרטים על `xmllint` וכלים נוספים של XML.
