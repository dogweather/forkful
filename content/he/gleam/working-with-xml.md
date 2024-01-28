---
title:                "עבודה עם XML"
date:                  2024-01-26T04:31:57.347082-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת ניתוח, שינוי ויצירת מסמכי XML, אשר משמשים להחלפת נתונים בשל הפורמט המובנה והנפוץ שלהם. מתכנתים מתמודדים עם XML כדי להתממשק עם מערכות רבות בהן XML היא לשון הקודש של הנתונים.

## איך לעשות:
Gleam לא תומך באופן ייחודי ב-XML, לכן נשתמש בספרייה חיצונית כמו `gleam_xml`. ראשית, הוספו אותה ל-`gleam.toml` שלכם:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

עכשיו, ניתוח ויצירת XML:

```rust
import gleam/xml

// ניתוח XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// יצירת XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

פלט לדוגמה עבור `xml.render(node)` הוא:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## טבילה עמוקה
XML הוא ראשי תיבות של eXtensible Markup Language, תקן מ-W3C כאחות ל-HTML. הוא קיים מאז סוף שנות ה-90. עבור Gleam, טיפול ב-XML נראה כמו צעד אחורה בזמן. JSON ו-Protocol Buffers הם יותר במגמה, אך השימוש הרחב ב-XML במערכות ישנות ובתעשיות מסוימות משמעו שהוא עדיין רלוונטי.

חלופות כמו `xmerl` קיימות באקוסיסטם של Erlang; עם זאת, הספרייה `gleam_xml` מספקת גישה יותר אידיומטית עבור משתמשי Gleam. היא בנויה על גבי ספריות Erlang קיימות אך מציעה ממשק API ידידותי ל-Gleam. הגישה של Gleam ל-XML שואפת לפשטות ולבטיחות, מפחיתה את הבלק "boilerplate" ומדגישה בטיחות טיפוסית.

מבחינת היישום, ספריות XML כוללות `gleam_xml` מספקות בדרך כלל מבנים דומים ל-DOM. זה כולל צמתים, מאפיינים ואלמנטים מקוננים, תוך ליבון של דפוסי ההתאמה והמקביליות של Erlang כדי להתמודד עם מסמכים גדולים ומורכבים במיוחד.

## ראה גם
- הספרייה `gleam_xml` ב-Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- התקן XML הרשמי מ-W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- מדריך מקיף ל-XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- תיעוד של `xmerl` של Erlang לעיבוד XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
