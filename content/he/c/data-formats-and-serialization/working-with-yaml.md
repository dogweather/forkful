---
title:                "עבודה עם YAML"
aliases: - /he/c/working-with-yaml.md
date:                  2024-02-03T18:14:29.404884-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שעומד ל-"YAML Ain't Markup Language," הוא תקן סידור נתונים קריא לאדם שניתן להשתמש בו למגוון יישומים, החל מקבצי תצורה ועד אחסון נתונים. תוכניתנים לעיתים קרובות עובדים עם YAML כאשר הם זקוקים לפורמט קל לקריאה וקל לכתיבה עבור קבצי תצורה או החלפת נתונים בין שפות ומערכות.

## איך לעשות:

עבודה עם YAML ב-C דורשת ספרייה, שכן הספרייה הסטנדרטית של C אינה מספקת תמיכה ישירה לפירסום או ניתוח YAML. אחת מספריות ה-YAML הפופולריות ביותר עבור C היא `libyaml`, שמציעה ממשקים ברמה נמוכה וברמה גבוהה לניתוח והפקת YAML. להלן דוגמה איך לנתח קובץ YAML פשוט באמצעות `libyaml`:

**ראשית**, עליך להתקין את ספריית ה-`libyaml`. אם אתה נמצא במערכת דומה ל-Unix, בדרך כלל תוכל להתקין אותה דרך מנהל החבילות שלך. לדוגמה, באובונטו:

```bash
sudo apt-get install libyaml-dev
```

**לאחר מכן**, שקול קובץ YAML פשוט בשם `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**הנה** דוגמה בסיסית איך לנתח את קובץ YAML זה ב-C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize YAML parser!\n", stderr);

    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Value: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

תוכנית פשוטה זו פותחת קובץ YAML, מאתחלת את מנתח ה-YAML, וקוראת את הקובץ, מדפיסה את ערכי הסקלר (בדוגמה זו, השדות של ה-YAML הפשוט שלנו). שים לב שבדיקת השגיאות היא מינימלית בדוגמה פשוטה זו וצריכה להיות יותר מבוססת בקוד ייצור.

הרצת התוכנית עם ה-`config.yaml` שלנו תפיק:

```plaintext
Value: John Doe
Value: 29
Value: false
```

## ניתוח מעמיק

YAML פורסם לראשונה ב-2001 ותוכנן להיות יותר קריא ונוח למשתמש מאשר פורמטים אחרים של סידור נתונים כמו XML או JSON, תוך שאב השראה ממספר שפות כמו C, Perl, ו-Python לפילוסופיית העיצוב שלו. למרות היתרונות שלו בקריאות ובקלות התיקונים על ידי אנשים, ניתוח YAML באופן תכנותי יכול להיות מורכב בזכות ההתמכות שלו בהזחה ובסט התכונות הרחב שלו, כולל הפניות וטיפוסים מותאמים אישית.

למרות ש-`libyaml` מספקת גישה מוצקה וברמה נמוכה לניתוח והפקת YAML ב-C, היא עשויה להיות מסורבלת למשימות פשוטות בזכות ה-API המפורט שלה. מסיבות אלו, חלק מהמתכנתים מעדיפים להשתמש בספריות ברמה גבוהה יותר או אפילו בפורמטים אחרים של סידור נתונים כמו JSON כאשר הם עובדים ב-C, במיוחד כאשר ניתוח ביצועים עם עודף קוד מינימלי הוא עדיפות. עם זאת, YAML נשאר בחירה פופולרית עבור קבצי תצורה ומצבים שבהם קריאות האדם מהווה עדיפות עליונה. אלטרנטיבות כמו TinyYAML או הטמעת מפרש ברמה גבוהה (למשל, הטמעת Python או Lua) עשויות לספק נוחות רבה יותר ליישומים ספציפיים, מאזנות בין נוחות השימוש לבין צרכי הביצועים.
