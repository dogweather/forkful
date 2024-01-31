---
title:                "עבודה עם XML"
date:                  2024-01-26T04:30:51.569145-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML פירושה להתמודד עם נתונים בפורמט מובנה ונפוץ, שנמצא בשימוש בהגדרות, הודעות, ועוד. מתכנתים מתעסקים ב-XML כדי לקרוא, לכתוב, לעדכן, ולשאול נתונים - חיוני לאינטרופרביליות בהמון אפליקציות ושירותים.

## איך ל:
Fish אינו כולל פרסור XML מובנה, לכן תצטרך להסתמך על כלים חיצוניים כמו `xmllint` או `xmlstarlet`. הנה קטע קוד לקריאת ערכים:

```fish
# ניתוח XML באמצעות xmlstarlet
echo '<root><element>שלום עולם</element></root>' | xmlstarlet sel -t -v "/root/element"
```

פלט:
```
שלום עולם
```

לעריכת XML, השתמשו בזה:

```fish
# עריכת אלמנט XML באמצעות xmlstarlet
echo '<root><element>ערך ישן</element></root>' | xmlstarlet ed -u "/root/element" -v 'ערך חדש'
```

פלט:
```xml
<?xml version="1.0"?>
<root>
  <element>ערך חדש</element>
</root>
```

## חקירה עמוקה:
XML קיים מאז שנות ה-90 המאוחרות, נוצר לקריאות ולידידות למכונה. על אף ש-JSON גזל חלק מהפופולריות של XML בזכות הפשטות שלו, XML עדיין נמצא בשימוש נרחב שם שוולידציה של מסמכים ו-namespaces הם מפתח. 

אלטרנטיבות? בטח - JSON, YAML, או אפילו פורמטים בינאריים כמו Protocol Buffers עבור אותן אפליקציות שדורשות ביצועים גבוהים. אבל הסכמה (schema) של XML ו-XSLT (עבור המרות XML) יכולים להיות שוברי שוויון לסצנריות מורכבות שבהן החוסן משנה.

מאחורי הקלעים, כלים כמו `xmlstarlet` מעטרים ספריות חזקות כמו libxml2, ונותנים לך XPath ו-XQuery לטפל ב-XML בצורה מדויקת. אלו לא רק כלים ל-XML אלא שערים למניפולציה של DOM, כפי שתיישם מושגים דומים בכל שפה שמגעת ב-XML.

## ראו גם:
- [תיעוד xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [תיעוד Fish](https://fishshell.com/docs/current/index.html)
- [פונקציות ואופרטורים של XPath ו-XQuery](https://www.w3.org/TR/xpath-functions/)
