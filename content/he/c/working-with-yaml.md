---
title:                "C: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## עבור מה?

קובצי YAML הם קבצי טקסט שמשמשים לניתוח ותיעוד של נתונים מבלי להיות קשורים לפורמט מסוים. מדוע שימוש במתאם YAML יהיה שימושי עבורך? יתרונות של עבודה עם YAML כוללים תיעוד קל יותר, התאמה גמישה יותר של מבני הנתונים, ואפילו יכולת לעבוד עם קבצים מקרובים מה שמקל על טיפול בתקלות.

## איך לעבוד עם YAML בלולאות

```C
#include <stdio.h>
#include "yaml.h"

int main() {
    FILE *file = fopen("example.yaml", "r"); // פתיחת קובץ YAML לקריאה
    yaml_parser_t parser;
    yaml_parser_initialize(&parser); // הכנת המתאם לפעולה
    yaml_parser_set_input_file(&parser, file); // הגדרת הקובץ כקלט למתאם
    yaml_event_t event;

    do {
        yaml_parser_parse(&parser, &event); // ניתוח הקובץ לפי אירועים
        switch (event.type) {
            case YAML_SEQUENCE_START_EVENT:
                printf("פתיחת רשימה. יש לנו %d איברים\n", event.data.sequence_start_start);
                break;
            case YAML_MAPPING_START_EVENT:
                printf("פתיחת מפתחות. יש לנו %d מפתחות\n", event.data.mapping_start_start);
                break;
            case YAML_SCALAR_EVENT:
                printf("ראו טוקן: %s\n", event.data.scalar.value); // הפסקה יחידה של טקסט בפייתון היא טוקן
                break;
        }
        yaml_event_delete(&event);
    } while (event.type != YAML_STREAM_END_EVENT);
    yaml_parser_delete(&parser); // סיום פעולת הקריאה
    fclose(file);
    return 0;
}
```

הנה כמה נתונים נוספים מקובץ YAML כדי להדגים את הפלט המצורף:

```yaml
- ארגמן: האיש
- מינשטרל: חג סחור סלקטור
```

בפלט המתקבל, יש שתי תכונות עבור רשומה נתונה:

```
ראו טוקן: ארגמן
ראו טוקן: האיש
```

וכך נעלמת החוצפה. איזה מירבינציה זו!

## העומק של YAML

לאורך הפוסט הזה, ראינו כי עבודה עם קבצי YAML מציעה שפע של יתרונות, ו