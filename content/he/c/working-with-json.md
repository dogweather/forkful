---
title:                "עובדים עם json"
html_title:           "C: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-json.md"
---

{{< edit_this_page >}}

מה ולמה?
עבודה עם JSON היא תהליך שבו מתבצעת התמרת מידע מפורמטים אחרים לתבניות JSON ולהפך. תהליך זה נהדר עבור מפתחי תוכנה בגלל יעילות ופשטות העבודה עם תבניות נתונים כולל. 

כיצד לעשות:
 ישנן כמה דרכים לעבוד עם JSON בשפת C. אחת הדרכים היא להשתמש בספריית JSON-C, שתוכננה כדי לתמוך בעבודה עם תבניות JSON בקוד C. הנה דוגמה קטנה של איך זה נראה:

```C
#include <stdio.h>
#include <json-c/json.h>
 
int main() {
  // יצירת משתנה מסוג json_object
  json_object *jobj = json_object_new_object();
  // הוספת מאפיין וערך למשתנה
  json_object_object_add(jobj, "name", json_object_new_string("John"));
  // הדפסת התוצאה
  printf("%s", json_object_to_json_string(jobj));
  // התחרטתי על הכפילות, נמחק!
  json_object_put(jobj);
  return 0;
}

// תוצאה:
// { "name": "John" }
```

עוד דרך לעבוד עם JSON ב-C היא להשתמש בספריית cJSON שמאפשרת פעולות נוספות עם תבניות נתונים מסוג JSON. כדי להשתמש בספרייה זו, נדרש להוריד את הקובץ cJSON.c והגירסה המתאימה של cJSON.h מהאתר הרשמי ולכלול אותם בקובץ ה-C שלנו.

עוד דוגמא לשימוש בספריית cJSON להדפסת תבנית JSON:

```C
#include <stdio.h>
#include <stdlib.h>
#include "cJSON.h"
 
int main() {
  // כיוון ששימוש ב-cJSON מסובך יחסית, נאתחל כאן
  char *json;
  cJSON *root = cJSON_CreateObject();
  cJSON *person = cJSON_CreateObject();
  // הגדרת אובייקט
  cJSON_AddStringToObject(person, "name", "John");
  cJSON_AddStringToObject(person, "address", "123 Main Street");
  // הוספת האובייקט לאובייקט הכללי
  cJSON_AddItemToObject(root, "person", person);
  // המרת התוצאה למחרוזת והדפסתה
  json = cJSON_Print(root);
  printf("%s\n", json);
  // נזהה ונכנס לספריית cJSON, נמחק!
  cJSON_Delete(root);
  // נשיחרר את המחרוזת
  free(json);
  return 0;
}

//תוצאה:
// { "person": { "name": "John", "address": "123 Main Street" } }
```

לצורך הפרדת העבודה עם JSON מהקוד הראשי, ניתן לשים קובץ שנקרא json_operations.c המכיל את כל הפונקציות המטפלות בעבודה עם JSON ולקרא אותם מקובץ ראשי יותר קצר ומסודר.

דריסת הערכים בתוך תבניות JSON יכולה להיות יפהפייה עם שימוש ברקורסיה כפי שמתואר בדוגמאות המצורפות באתרים הרשמיים של הספריות.

התױ נמיה!:
תהליך עבודה עם JSON גמיש ופשוט. כי לאף אחת מהנוכחיות שלנו לעשות את כל העבודה בעצמה, ניתן להשתמש בספריות מוכנות המאפשרות המרה קלה של נתונים לתבניות JSON ולהיפך, כך שנוכל להתמקד בכיתוב