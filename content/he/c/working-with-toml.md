---
title:                "עובדים עם TOML"
aliases:
- he/c/working-with-toml.md
date:                  2024-02-03T18:13:13.079711-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

TOML (Tom's Obvious, Minimal Language - שפת התצורה המינימלית והברורה של טום) היא פורמט קובץ תצורה שקל לקרוא בעקבות הסמנטיקה הברורה שלו. מתכנתים משתמשים בו לקבצי תצורה ביישומים כי פשטותו והקריאות שלו מהווה בחירה מעולה בהשוואה לפורמטים כמו XML או JSON בהקשרים מסוימים.

## איך לעשות:

כדי לעבוד עם TOML ב-C, תצטרך תחילה ספרייה שיכולה לפענח קבצי TOML, מכיוון שהספרייה הסטנדרטית של C לא כוללת פונקציונליות זו. בחירה פופולרית היא `tomlc99`, פענח TOML קל משקל ל-C99. הנה מדריך מהיר לקרוא קובץ תצורה TOML פשוט:

ראשית, ודא ש`tomlc99` מותקן ומקושר כראוי בפרויקט שלך.

**דוגמת קובץ TOML (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**קוד C לפיענוח קובץ זה:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Database Server: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**פלט:**
```
Database Server: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## טבילה עמוקה

TOML נוצר על ידי טום פרסטון-ורנר, שותף מייסד של GitHub, כתגובה למגבלות שהוא הבחין בהן בפורמטים אחרים של קבצי תצורה. מטרתו היא להיות פשוטה וחד-משמעית, הן לאנשים והן למחשבים, לקרוא ולכתוב בלי הצורך בכללי פענוח מורכבים. באקוסיסטם של C, TOML אינו אזרח דרג א' כמו שהוא עשוי להיות בשפות גבוהות יותר כמו Rust עם `serde_toml` או Python עם `toml`, שיש להן ספריות עם תמיכה ילידית. לעומת זאת, מפתחי C צריכים להסתמך על ספריות חיצוניות כמו `tomlc99`, אך זהו מצב טיפוסי בהתחשב בדגש של C על מינימליזם וביצועים.

למרות ש-TOML מקבל שבחים על בהירותו, כאשר בוחרים פורמט קובץ תצורה, חיוני לשקול את צרכי הפרויקט. בתרחישים הדורשים מבנים מורכבים יותר או אינטראקטיביות עם web APIs, JSON או אפילו YAML עשויים להציע התאמה טובה יותר למרות המורכבות המוגברת שלהם. TOML מבריק בתצורות שבהן הקריאות והפשטות הם העולים על הכל, לא בהכרח שם שיש צורך במבני נתונים המתקדמים ביותר.
