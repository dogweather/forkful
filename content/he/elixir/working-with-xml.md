---
title:                "עבודה עם XML"
date:                  2024-01-26T04:30:07.521248-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML ב-Elixir משמעה ניתוח, יצירה וניהול של נתוני XML. מתכנתים עוסקים ב-XML מפני שהוא נפוץ בשירותי אינטרנט, קבצי הגדרות ומערכות ישנות.

## איך לעשות זאת:
Elixir אינה כוללת ניתוח XML בספריה הסטנדרטית שלה. SweetXML הוא בחירה פופולרית. הנה איך להשתמש בו:

```elixir
# הוסף את SweetXML לתלותייך ב-mix.exs
{:sweet_xml, "~> 0.6"}

# בקוד שלך
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# ניתוח XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # פלט: Tove
```

## צלילה לעומק
XML, או Extensible Markup Language, קיים מאז סוף שנות ה-90. הוא מפורט אך מובנה - אידיאלי להחלפת נתונים מורכבים. בעוד שהפופולריות של JSON טסה בשמיים בזכות פשטותו, XML נותר מושרש במערכות רבות עסקיות ופיננסיות בזכות ביטוייותו וסכמות מתוקנות.

אלטרנטיבות כוללות:
- JSON להחלפת נתונים פחות מפורטת וקלה יותר.
- Protobuf או Thrift לתקשורת נתונים מסוגלת בינארית, בעיקר למערכות פנימיות.

מאחורי הקלעים, ספריות XML עבור Elixir מנצלות את ספריית ה-:xmerl של Erlang לניתוח, אשר מספקת תמיכה חזקה אך יכולה להיות פחות אינטואיטיבית מגישות מודרניות יותר. ככל ש-Elixir מתפתחת, ספריות מונעות קהילה כמו SweetXML מעטפות את אלו עם תחביר יותר Elixir-י, הופכות את הניהול של XML לגישה יותר נגישה.

## ראה גם:
- SweetXML ב-Hex: https://hex.pm/packages/sweet_xml
- התפיסה של Elixir על ניתוח XML: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- תיעוד xmerl עבור טיפול ב-XML מאחורי הקלעים: http://erlang.org/doc/apps/xmerl/index.html