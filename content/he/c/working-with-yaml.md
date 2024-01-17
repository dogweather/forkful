---
title:                "עבודה עם yaml"
html_title:           "C: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-yaml.md"
---

{{< edit_this_page >}}

מה ולמה?
עבור מי שלא מכיר, YAML הוא פורמט בינארי להעברת נתונים והגדרת הגדרות. כתיבת קוד בשפת C עם YAML יכולה להקל על איחזור וניהול נתונים באפליקציות, וזה נפוץ בקוד של ווב ואפליקציות ניידות.

איך לעשות:
המהדר של C כבר מגיע עם ספריית YAML כבר מוכנה לשימוש. כדי להתחיל, צריך רק להצהיר על עבוריות YAML ולקבל משתנה שיכיל את הנתונים שלנו. לפניכן ניתן למשלוח פקודות לקריאת רישומים או ליצור נתונים חדשים.

```C
#include <yaml.h>

yaml_parser_t parser;
yaml_event_t event;

yaml_parser_initialize(&parser);
FILE *input = fopen("my_data.yaml", "rb");

while (yaml_parser_parse(&parser, &event))
{
  // handle events here
}

```

דוגמאות לפעולות נוספות ופלט נתונים ניתן למצוא במדריך המפורט של YAML בעמוד הרישמי.

התרגול העמוק:
YAML נוצר בשנת 2001 כפורמט תקיות לשפה הרפאל, כיום הוא מקבל תמיכה של רבים ומשתמשים כמובן. יכולות נוספות כדוגמת JSON ו XML מאפשרות בחירה בין פורמט לפי הצורך. החזר פעולות ברצף כדי לאפשר ניהול נתונים מכל מקום בקוד.

 ראה גם:
כדי ללמוד עוד על שימושים וגם דוגמאות יעילות לתוכניות C עם YAML, נרצה להמליץ על המדריך הבא:
- התיעוד הרשמי של YAML
- הספריה של YAML לשפת C
- עמוד הוויקי של YAML בוויקיפדיה