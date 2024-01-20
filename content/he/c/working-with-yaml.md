---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט לשמירת והעברת נתונים, קריא לבני אדם ופשוט לניתוח על ידי מחשב. תכניתנים משתמשים בו להגדרות תצורה, תיעוד וסידור נתונים.

## איך לעשות:
כדי לעבוד עם YAML ב-C, נדרשת ספריית חיצונית. לדוגמה, להלן אופן השימוש בספריית `libyaml`:

```C
#include <yaml.h>

int main(void) {
    yaml_parser_t parser;
    yaml_document_t document;

    FILE *input = fopen("example.yaml", "rb");
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, input);

    if (yaml_parser_load(&parser, &document)) {
        // קידוד של פעולות עם המסמך
    }

    yaml_document_delete(&document);
    yaml_parser_delete(&parser);
    fclose(input);

    return 0;
}
```

בוצע קריאה וניתוח של מסמך YAML. זה בדוגמה פשוטה, הטיפול בנתונים צריך התאמה לסצנריו הקונקרטי שלך.

## עיון נוסף:
YAML הוא ראשי תיבות של YAML Ain't Markup Language. נולד ב-2001, הוא פופולרי בשל קריאותו ונוחותו לשימוש. אלטרנטיבות כוללות JSON או XML. בקוד C, יישום YAML יכול להיות מורכב יותר לעומת פורמטים אחרים, במיוחד בהיעדר ספרייה סטנדרטית לפענוח או יצירת YAML.

## ראה גם:
- [libyaml](https://github.com/yaml/libyaml) - ספריית C לניתוח וייצור YAML.
- [YAML Specification](https://yaml.org/spec/1.2/spec.html) - המפרט הרשמי של YAML גרסה 1.2.
- [Learn YAML in Y Minutes](https://learnxinyminutes.com/docs/yaml/) - מדריך מהיר לסינטקס של YAML.