---
date: 2024-01-26 04:30:07.521248-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Elixir \u05DE\
  \u05E9\u05DE\u05E2\u05D4 \u05E0\u05D9\u05EA\u05D5\u05D7, \u05D9\u05E6\u05D9\u05E8\
  \u05D4 \u05D5\u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\
  \u05D9 XML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E1\u05E7\u05D9\
  \u05DD \u05D1-XML \u05DE\u05E4\u05E0\u05D9 \u05E9\u05D4\u05D5\u05D0 \u05E0\u05E4\
  \u05D5\u05E5 \u05D1\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8, \u05E7\u05D1\u05E6\u05D9 \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA\
  \ \u05D5\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA."
lastmod: '2024-03-13T22:44:38.813778-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1-Elixir \u05DE\u05E9\
  \u05DE\u05E2\u05D4 \u05E0\u05D9\u05EA\u05D5\u05D7, \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D5\u05E0\u05D9\u05D4\u05D5\u05DC \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\
  \ XML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E1\u05E7\u05D9\
  \u05DD \u05D1-XML \u05DE\u05E4\u05E0\u05D9 \u05E9\u05D4\u05D5\u05D0 \u05E0\u05E4\
  \u05D5\u05E5 \u05D1\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8, \u05E7\u05D1\u05E6\u05D9 \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA\
  \ \u05D5\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
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
