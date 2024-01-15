---
title:                "עבודה עם YAML"
html_title:           "C: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה:
יכול להיות שאתה נתקלת בקבצי YAML בעבודתך וצריך לעבוד איתם, או שאולי תרצה ליצור קוד שמייצג מידע בפורמט הזה. בכל מקרה, ידע כיצ׳ור ה- YAML יהיה שימושי עבורך.

## איך לעבוד עם YAML בשפת C:
ה-YAML הוא לכאורה פורמט קובץ פשוט, אך יש לו מאפיינים ייחודיים שיכולים להפוך אותו לאתגר כאשר מדובר בעיבוד ועיבוד מידע מורכב. הכירו את העולם של YAML בעזרת דוגמאות קוד ופלטים שלהלן:

```c
// טענת הספריות הדרושות
#include <yaml.h>

int main() {
	// יצירת מבנה לאחסון הנתונים
	yaml_document_t document;
	
	// טעינת הקובץ הקיים
	yaml_parser_t parser;
	yaml_parser_initialize(&parser);
	yaml_parser_set_input_file(&parser, "file.yaml");
	
	// קריאת הנתונים מהקובץ והצבתם בסטרים
	yaml_parser_load(&parser, &document);
	yaml_node_t *node = yaml_document_get_root_node(&document);
	
	// קבלת נתונים מאחת התוכן בעץ
	yaml_node_t *key = yaml_document_get_node(&document, node->data.mapping.pairs.start->key);
	char *value = (char*)node->data.mapping.pairs.start->value->data.scalar.value;
	
	// יידפוס התוכן למסך
	printf("מפתח: %s, ערך: %s\n", key->data.scalar.value, value);
	
	// קיצור המודול וסגירת היעד
	yaml_document_delete(&document);
	yaml_parser_delete(&parser);
	
	return 0;
}
```

הפלט:
```
מפתח: name, ערך: John Doe
```

## העמקה:
ישנם כמה נסיבות בהן ייתכן שהידע על YAML ישמשך כמה עבודה נוספת. לדוגמה, תמיכה ב-YAML נמצאת בשפות תכנות מגוונות כמו גולנג׳ וגיט, וניתן להמיר קבצי YAML לפורמט XML דרך קבלה מעופשת.

## ראו גם:
- [פרויקט ה-YAML הרשמי](https://yaml.org/)
- [תיעוד C של YAML](https://yaml.org/spec/1.2/spec.html)
- [טכנולוגיית ה-YAML בוויקיפדיה](https://en.wikipedia.org