---
title:                "עבודה עם XML"
date:                  2024-01-26T04:27:53.629784-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-xml.md"
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
