---
date: 2024-01-26 04:27:53.629784-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05DE\
  \u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\u05E4\u05D4 \u05D4\
  \u05E1\u05D9\u05DE\u05D5\u05DF \u05D4\u05E0\u05E8\u05D7\u05D1\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\u05DD\
  \ \u05E2\u05DD XML \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D6\u05D4\u05D5\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E0\u05E4\u05D5\u05E5 \u05DC\u05D4\u05D2\u05D3\u05E8\u05D5\
  \u05EA, API-\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:13.150598-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D5\u05DE\
  \u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\u05E4\u05D4 \u05D4\
  \u05E1\u05D9\u05DE\u05D5\u05DF \u05D4\u05E0\u05E8\u05D7\u05D1\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\u05D3\u05D9\u05DD\
  \ \u05E2\u05DD XML \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D6\u05D4\u05D5\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E0\u05E4\u05D5\u05E5 \u05DC\u05D4\u05D2\u05D3\u05E8\u05D5\
  \u05EA, API-\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת ניתוח, חילוץ ומניפולציה של נתונים בפורמט שפה הסימון הנרחבת. מתכנתים מתמודדים עם XML מכיוון שזהו פורמט החלפת נתונים נפוץ להגדרות, API-ים ועוד.

## איך לעשות:
הנה איך לנתח XML ב-Bash. כלים? xmllint ו-xmlstarlet. לולאה דרך אלמנטים של XML? בהחלט. דוגמא עם פלט דוגמא:

```bash
# בהנחה ש-xmlstarlet מותקן
# התקנה עם: apt-get install xmlstarlet

# ניתוח תוכן XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# חילוץ שמות עם xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# הפלט אמור להיות:
# Apple
# Banana
```

## צלילה עמוקה
בשנות ה-90, XML צץ כאלטרנטיבה פשוטה יותר ל-SGML, אך עם מבנה מסודר יותר מ-HTML. כעת, יש לו חברה – JSON, YAML, למשל. אבל XML עדיין בעיניין, במיוחד בהגדרות ושירותי אינטרנט מבוססי SOAP.

בכליים, xmllint נוח לאימות XML, שאילתות xpath. xmlstarlet הוא הסכין השוויצרי לטריקים של XML – שאילתה, עריכה, אימות, המרה. בסקריפטים של bash, הם גיבורי על למשימות XML.

מאחורי הקלעים, xmllint משתמש ב-libxml2 – המפענח C של XML. הוא מהיר, אבל הודעות השגיאה? אניגמטיות. ו-xmlstarlet? תבניות רקורסיביות ותמיכה ב-EXSLT. דורש מאמץ חשיבה, אבל עוצמתי.

## ראה גם
- [xmlsoft.org](http://xmlsoft.org/): פריטים של Libxml2 ו-xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): בעיות ופתרונות מהעולם האמיתי.
- [W3Schools קורס XML](https://www.w3schools.com/xml/): היסודות של XML.
