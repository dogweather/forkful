---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:04.785788-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1-Java, \u05E0\u05D9\u05EA\u05DF \u05DC\
  \u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 YAML \u05D1\u05D0\
  \u05DE\u05E6\u05E2\u05D5\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3\
  \ \u05E9\u05DC\u05D9\u05E9\u05D9, \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\
  \u05DE\u05D4\u05D3\u05D5\u05E8\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\
  \u05EA \u05E9\u05DC Java \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-YAML.\
  \ \u05E1\u05E4\u05E8\u05D9\u05D4 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05EA\
  \ \u05D4\u05D9\u05D0\u2026"
lastmod: '2024-03-13T22:44:39.165723-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Java, \u05E0\u05D9\u05EA\u05DF \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\
  \u05DD \u05E7\u05D1\u05E6\u05D9 YAML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9\
  , \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05DE\u05D4\u05D3\u05D5\u05E8\
  \u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05E9\u05DC Java\
  \ \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\
  \u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-YAML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

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
