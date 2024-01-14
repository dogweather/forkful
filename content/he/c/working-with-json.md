---
title:                "C: עובדים עם ג'ייסון"
simple_title:         "עובדים עם ג'ייסון"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-json.md"
---

{{< edit_this_page >}}

## מדוע

יצירת קשר עם אפליקציות מקוונות ושימוש במידע מאוחסן אינו אפשרי בלעדי עבודה עם JSON. קבצי JSON מאפשרים לנו לאחזר נתונים מבלי להיות תלויים בתפתחות הטכנולוגיות השונות של אפליקציות נמצאות באינטרנט.

## איך לעשות זאת

תחילה, נדרוס את הקובץ הנתונים שלנו למחרוזת ונשתמש בפונקציות `json_parse()` ו`json_value_get()` כדי לקרוא וליצור מבנה מתאים לכל ערך בקובץ. לאחר מכן, נשתמש בלולאה `json_object_foreach()` כדי לעבור על כל המפתחות והערכים של המבנה ולהדפיס אותם בפורמט שנרצה. הנה דוגמא של קוד ופלט לקובץ JSON פשוט:

```C
#include <stdio.h>
#include "json-c/json.h"

int main() {
    FILE *fp = fopen("data.json", "r");
    if (!fp) {
        printf("לא ניתן היה לפתוח קובץ!\n");
        return 1;
    }
    char buffer[1024];
    struct json_object *parsed_json;
    struct json_object *name;
    
    fgets(buffer, 1024, fp);
    parsed_json = json_tokener_parse(buffer);
    json_parse(&parsed_json, "name", &name);
    
    printf("שם: %s\n", json_value_get_string(name));
    fclose(fp);
    return 0;
}
```

פלט:

```
שם: ים
```

## צלילה עמוקה

עבודה עם JSON יכולה להיות יותר מורכבת כאשר מדובר בטיפוסים מסובכים, כמו מערכים או מבני נתונים מקוננים. ניתן להשתמש בפונקציות נוספות כדי לטפל במקרים אלו, כמו `json_object_array_length()` כדי לקבוע את אורך המערך ו`json_object_object_add()` כדי להוסיף נתונים חדשים למבנה הנתונים. בנוסף, ישנם כלים נוספים כמו JSON Schema ו-JSON Validator שיכולים לעזור בבדיקה ואימות מבנה הנתונים שלנו כדי לוודא שהכל תקין ומתאים.

## ראה גם

- [הדרכות שימוש ב