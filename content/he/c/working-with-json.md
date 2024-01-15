---
title:                "עבודה עם json"
html_title:           "C: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON הוא שפת פריסת מידע פופולרית ונמצאת בשימוש נרחב בתחום התכנות. כשמשתמשים ב־C כדי לעבוד עם JSON, ניתן ליצור יישומים מוצלחים ויעילים שמשתמשים במודל הנתונים הפשוט והקל לקריאה של JSON.

## איך לעבוד עם JSON ב־C

הליכים פשוטים למדי לעבוד עם JSON ב־C:

### התחברות לספריית JSON

כדי להתחיל לעבוד עם JSON ב־C, תחילה עלינו להוריד ולהתקין את הספרייה הנדרשת. הנה דוגמה של איך ניתן לעשות זאת ב־Ubuntu:

```C
sudo apt-get install libjson-c-dev
```

### יצירת מופע JSON

כדי ליצור מופע JSON חדש, ניתן להשתמש בפונקציה `json_object_new_object()`. כאן יש דוגמה לכיצד ליצור מופע JSON ולהדפיס אותו:

```C
json_object *json = json_object_new_object();
printf("Object created: %s", json_object_to_json_string(json));
```

### גישה לערכים במופע JSON

ניתן לגשת לערכים במופע JSON על ידי שימוש ב־`json_object_object_get()`. כאן תוכלו למצוא דוגמה לכיצד לגשת לערכים במופע ולהדפיס אותם:

```C
// נתון מופע עם מפתח name וערך John
json_object *json = json_object_new_object();
json_object_object_add(json, "name", json_object_new_string("John"));

// גישה לערך של 'name'
json_object *name = json_object_object_get(json, "name");
// הדפסת הערך של 'name'
printf("Name: %s", json_object_get_string(name));
```

### קריאת נתונים מ־JSON קיים

כאשר רוצים לקרוא נתונים מ־JSON קיים, נדרש להתחבר לקובץ ה־JSON ולהפעיל את הפעולות הרצויות. הנה דוגמה לכיצד לעשות זאת:

```C
// פתיחת קובץ JSON קיים
json_object *json = json_object_from_file("example.json");

// גישה לערכים במופע
json_object *name = json_object_object_get(json, "name");
// הדפסת הערך של 'name'
printf("Name: %s", json_object_get_string(name));
```

### כתיבת נתונים ל־JSON חדש