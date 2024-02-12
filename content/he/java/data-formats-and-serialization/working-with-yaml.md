---
title:                "עבודה עם YAML"
aliases:
- /he/java/working-with-yaml.md
date:                  2024-02-03T19:26:04.785788-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
YAML, ראשי תיבות של "YAML Ain't Markup Language," הוא תקן לסריאליזציה של נתונים קריאה לאדם שמתכנתים משתמשים בו עבור קבצי תצורה, ייצוא נתונים, והעברת נתונים בין שפות. הוא פופולרי בזכות הקריאות והנוחות שלו, מה שהופך אותו לבחירה נפוצה לתצורה של יישומים ושירותים.

## איך ל:
ב-Java, ניתן לעבוד עם קבצי YAML באמצעות ספריות צד שלישי, מכיוון שהמהדורה הסטנדרטית של Java אינה כוללת תמיכה מובנית ל-YAML. ספריה פופולרית היא SnakeYAML, שמאפשרת ניתוח ויצירה של נתוני YAML בקלות.

### הגדרת SnakeYAML
ראשית, כלול את SnakeYAML בפרויקט שלך. אם אתה משתמש ב-Maven, הוסף את התלות הבאה ל-`pom.xml` שלך:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### קריאת YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
בהנחה ש-`config.yml` נראה כך:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
הפלט יהיה:
```
{name=Example, version=1.0, features=[login, signup]}
```

### כתיבת YAML
כדי לייצר YAML מאובייקטים של Java, השתמש במתודת `dump` שמספקת SnakeYAML:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
זה יייצר וידפיס את תוכן ה-YAML הבא:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
על ידי ניצול SnakeYAML, מפתחי Java יכולים בקלות לשלב ניתוח ויצירת YAML באפליקציות שלהם, נהנים מהקריאות והפשטות של YAML למטרות תצורה והחלפת נתונים.
