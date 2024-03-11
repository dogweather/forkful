---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:34.655224-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object\
  \ Notation) \u05D1\u05E9\u05E4\u05EA C \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\
  \u05EA\u05D5\u05D7, \u05D9\u05E6\u05D9\u05E8\u05D4 \u05D5\u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC\
  \ JSON. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05EA\u05E7\
  \u05E9\u05D5\u05E8\u05EA \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E0\u05D8,\u2026"
lastmod: '2024-03-11T00:14:13.675847-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object Notation)\
  \ \u05D1\u05E9\u05E4\u05EA C \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\
  \u05D7, \u05D9\u05E6\u05D9\u05E8\u05D4 \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9 \u05DE\
  \u05D1\u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC JSON. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05EA\u05E7\u05E9\u05D5\
  \u05E8\u05EA \u05E2\u05DD \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8,\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON (JavaScript Object Notation) בשפת C כוללת ניתוח, יצירה ושינוי מבני נתונים של JSON. מתכנתים עושים זאת כדי לאפשר תקשורת עם שירותי אינטרנט, אחסון נתונים או קבצי הגדרות בפורמט קליל וקריא לאדם.

## איך לעשות:

כדי לעבוד עם JSON ב-C, בדרך כלל תשתמשו בספרייה כמו `jansson` או `json-c` עקב העדר תמיכה פנימית ב-JSOn בשפת C. כאן, נתמקד ב-`jansson` בשל קלות השימוש והתחזוקה הפעילה שלה. ראשית, התקינו את הספרייה (למשל, באמצעות מנהל החבילות `apt` באובונטו: `sudo apt-get install libjansson-dev`).

בואו נתחיל בניתוח מחרוזת JSON וגישה לתוכן שלה:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Name: %s\nAge: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

תוצאת דוגמה:
```
Name: John Doe
Age: 30
```

לאחר מכן, יצירה ופלט של אובייקט JSON:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

תוצאת דוגמה:
```
{"name": "Jane Doe", "age": 25}
```

דוגמאות אלו מדגימות את היסודות של טעינת מחרוזת JSON, פרוק ערכיה, יצירת אובייקט JSON חדש, ולאחר מכן פליטתו כמחרוזת.

## ניתוח מעמיק

הצורך לעבוד עם JSON ב-C נובע מקבלת הווב את JSON כפורמט ראשי להחלפת נתונים. הפשטות והיעילות של JSON הפכו אותו מהר מאוד לפורמט המועדף על פני XML, למרות היעדר התמיכה הישירה של C בניהול JSON בתחילה. פתרונות ראשוניים כללו ניהול מחרוזות ידני – תהליך שגוי ולא יעיל. ספריות כמו `jansson` ו-`json-c` הופיעו כדי למלא את הפער, והן מספקות API חזק לניתוח, בנייה, וסידור של JSON.

בעוד ש-`jansson` מציעה פשטות וקלות שימוש, `json-c` עשויה למשוך את אלו המחפשים סט יכולות רחב יותר. עם זאת, אלטרנטיבות כמו ספריות ניתוח ב-C++ מציעות אבסטרקציות מתוחכמות יותר, בזכות מבני נתונים מורכבים יותר ותמיכת ספרייה סטנדרטית בשפה זו. עם זאת, כאשר עובדים בסביבות בהן C היא השפה המועדפת או הנדרשת – כגון מערכות מוטבעות או בעת יצירת ממשק לספריות C קיימות – שימוש ב-`jansson` או `json-c` הופך להכרחי.

כדאי גם לציין כי עבודה עם JSON ב-C כוללת הבנה עמוקה של ניהול זיכרון, מכיוון שהספריות הללו לעיתים מחזירות אובייקטים שהוקצו דינמית ודורשים נטרול מפורש. זה מאתגר את המתכנתים לשקול בין נוחות לאחריות למניעת נזילות זיכרון, מרכיב קריטי ביצירת קוד C יעיל.
