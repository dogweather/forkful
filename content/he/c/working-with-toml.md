---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:20:17.712660-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML היא שפת הסריאליזציה של נתונים שתוכננה להיות קלה לקריאה ולכתיבה. תכנתים משתמשים בה לקבצי תצורה, אחסון נתונים פשוט, וחילופי נתונים בין-שפות בעקבות הבהירות והאדיבות האנושית שלה.

## איך ל:
בואו ננתח קובץ תצורה ב-TOML ב-C באמצעות הספרייה "tomlc99". תחילה, התקנו את הספרייה. לאחר מכן, צרו קובץ `config.toml`:

```toml
title = "דוגמה ל-TOML"

[owner]
name = "טום פרסטון-ורנר"
dob = 1979-05-27T07:32:00Z
```

כעת, ננתח אותו ב-C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("שגיאה: לא ניתן לפתוח את קובץ התצורה\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("שגיאה: %s\n", errbuf);
        return 1;
    }

    printf("כותרת: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("שם הבעלים: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
פלט לדוגמה:
```
כותרת: "דוגמה ל-TOML"
שם הבעלים: "טום פרסטון-ורנר"
```

## ניתוח עמוק
TOML, שהוא ראשי תיבות של Tom's Obvious, Minimal Language, נוצר על ידי טום פרסטון-ורנר ב-2013. הוא משמש כחלופה פשוטה יותר לפורמטים כמו XML ו-YAML, ומתמקד בלהיות יותר קריא ונכתב עבור בני אדם. למרות ש-JSON הוא חלופה נוספת, TOML שומר על מבנה שקל לפרס אותו באופן חזותי על ידי בני אדם, מה שהוא אחד הסיבות העיקריות לאמצו בקבצי תצורה.

ב-C, עבודה עם TOML כוללת בחירה של ספריית ניתוח מכיוון שהשפה אינה תומכת בה ילידית. ספריות כמו "tomlc99" הן תואמות C99 ומספקות API לפענוח טקסט TOML. כאשר מתחשבים בביצועים, טיפול נכון בשגיאות וניהול זיכרון הם קריטיים מאחר ש-C אינה מכילה איסוף זבל מובנה.

## ראו גם:
1. מפרט TOML: [https://toml.io/en/](https://toml.io/en/)
2. מאגר GitHub של tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. השוואת פורמטים של סריאליזציה של נתונים: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
