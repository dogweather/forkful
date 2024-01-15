---
title:                "עבודת יימל:"
html_title:           "Java: עבודת יימל:"
simple_title:         "עבודת יימל:"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

בקוד המודרני, יתרון גדול שלהכיל נתונים מורכבים בקבצים שונים כדוגמת YAML ניתן לחלוטין לוודא את הנכונות של נתונים ולשמור על קוד נקי ומסודר.

## כיצד לעשות זאת

### התקנת ספריית YAML 

כדי לעבוד עם YAML בקוד Java, יש להתקין את הספרייה הרלוונטית בעזרת Maven או Gradle. ניתן להתקין את הספרייה הבאה על ידי הוספת התלות הבאה לקובץ ה-pom שלך:

```Java
<dependency>
  <groupId>org.yaml</groupId>
  <artifactId>snakeyaml</artifactId>
  <version>1.26</version>
</dependency>
```

#### יצירת מפתחת YAML חדשה

ניתן ליצור מפתחת חדשה בתוך קוד Java כדלקמן:

```Java
// ייבוא מחלקת המפתחת
import org.yaml.snakeyaml.Yaml;

// יצירת מפתחת חדשה
Yaml yaml = new Yaml();
```

#### כתיבת YAML

עם המפתחת, ניתן לכתוב נתונים לקובץ YAML בעזרת הפונקציה `dump()`:

```Java
// יציאת הנתונים לקובץ YAML
String output = yaml.dump(data);
System.out.println(output);
```

הפונקציה `dump()` מאפשרת גם להגדיר הגדרות נתונים מתקדמות כדוגמת סדר סחירה ובחירת ערכים חכמים. לדוגמה, מפתחת זו תמיד תיצור ערכים חדשים במיוחד:

```Java
// כתיבת YAML עם סידר סחירה וערכים חכמים
yaml.dump(data, new DumperOptions().setDefaultFlowStyle(FlowStyle.BLOCK));
```

כדי לכתוב ערכים חכמים בקובץ YAML, ניתן להשתמש ב- `PropertyUtils`:

```Java
// כתיבת ערכים חכמים עם PropertyUtils
PropertyUtils output = Factory.getDefaultPropertyUtils();
Map<String, Object> map = output.map("key123", new Object());
```

#### קריאת YAML

בקובץ YAML ניתן לקרוא נתונים באמצעות הפונקציה `load()`:

```Java
// קריאת נתונים מתוך קובץ YAML
String input = "key123: string";
Map<String, Object> map = (Map<String, Object>)yaml.load(input);