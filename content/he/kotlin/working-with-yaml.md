---
title:                "עבודה עם קובצי yaml"
html_title:           "Kotlin: עבודה עם קובצי yaml"
simple_title:         "עבודה עם קובצי yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

בזמן האחרון, YAML נהפך להיות תבנית פופולרית לכתיבת קבצי תצורה ונתונים בפרויקטים תוכנה. אם אתה מעוניין לעבוד עם YAML, אתה צריך להכיר את הקוד שכתוב בשפת תכנות Kotlin, ולשמור על החיבור שלך עם הפרויקטים תוכנה שונים.

## איך לעשות זאת

תתחיל מהפכת העבודה עם YAML ב-Kotlin, אתה תצטרך להתקין את הספרייה "snakeyaml" באמצעות כלי הניהול של הפרויקט שלך. לאחר מכן, תהליך זה ניתן לכתיבת פעולות שונות כגון קריאת, כתיבת, ועדכון של קובץ YAML באמצעות Kotlin.

```Kotlin
// קריאת קובץ YAML
val yamlString = File("test.yaml").readText()

// כתיבת נתונים לקובץ חדש
val data = hashMapOf<String, Any>("name" to "Kotlin", "version" to "1.4")
val newYaml = Yaml().dump(data)
File("new.yaml").writeText(newYaml)
```

```Kotlin
// עדכון נתונים בקובץ קיים
val yamlString = File("test.yaml").readText()
val yaml = Yaml()
val data = yaml.load(yamlString)

// עדכון שדה קיים
data["version"] = "1.5"

// כתיבת העדכון לקובץ
File("test.yaml").writeText(yaml.dump(data))
```

כעת, נראה את כמה נתונים שנמצאות בקובץ YAML:

```yaml
name: Kotlin
version: 1.5
```

ניתן לעבוד עם מבני נתונים מורכבים יותר בקובץ YAML, כמו מערכי מילים ורשימות.

```yaml
languages:
  - Java
  - Kotlin
  - Python

frameworks:
  - Spring Boot
  - Ktor
```

כעת, עם כל הידע שנרכש, תוכל לכתוב ולערוך קבצי YAML בשפת Kotlin בקלות.

## חפירה עמוקה

אם אתה מעוניין ללמוד עוד על הפורמט של YAML ועל הסיבות לשימוש בו, תוכל לקרוא עוד במקורות הלימוד הבאים:

- [מסמכי ייסוד של YAML](https://yaml.org/spec/1.2/spec.html)
- [מדריך קצ