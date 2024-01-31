---
title:                "עבודה עם JSON"
date:                  2024-01-19
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON (JavaScript Object Notation) הוא פורמט תקשורת נתונים פופולרי. מתכנתים משתמשים ב-JSON להמרת נתונים בין שרת ללקוח ובין שירותים באינטרנט כי זה פשוט, קריא ושפת אגנוסטי.

## איך לעשות:
```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    // יצירת אובייקט JSON
    json_object *new_obj = json_object_new_object();
    json_object *name = json_object_new_string("ירדן");
    json_object *age = json_object_new_int(30);

    // הוספת שדות לאובייקט
    json_object_object_add(new_obj, "שם", name);
    json_object_object_add(new_obj, "גיל", age);

    // הדפסת האובייקט
    printf("JSON object created: %s\n", json_object_to_json_string(new_obj));

    // ניקוי זיכרון
    json_object_put(new_obj);

    return 0;
}
```
תוצאה:
```
JSON object created: {"שם": "ירדן", "גיל": 30}
```

## צלילה לעומק
ב-JSON התחיל כחלק משפת JavaScript אך הפך לתקן מאודות בכל השפות. האלטרנטיבות כוללות XML ו-YAML, אך JSON נותר פופולרי בזכות פשטותו. ב-C, לעבוד עם JSON דורש lib like json-c או jansson. אלו מעניקים השפת API לסידור וניתוח JSON עם ניהול זיכרון אוטומטי.

## ראה גם
- המדריך הרשמי ל-json-c: https://json-c.github.io/json-c/
- מסמכי jansson: https://jansson.readthedocs.io/en/latest/
- מקורות ללמידת JSON: https://www.json.org/json-en.html
