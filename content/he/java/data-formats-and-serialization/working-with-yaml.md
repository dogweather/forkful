---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:04.785788-07:00
description: "YAML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\
  \u05DC \"YAML Ain't Markup Language,\" \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05DC\
  \u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\u05D0\u05D3\
  \u05DD \u05E9\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\u05D9\
  \ \u05EA\u05E6\u05D5\u05E8\u05D4, \u05D9\u05D9\u05E6\u05D5\u05D0 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA\u2026"
lastmod: '2024-02-25T18:49:37.401146-07:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\u05DC\
  \ \"YAML Ain't Markup Language,\" \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05DC\u05E1\
  \u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\u05D0\u05D3\u05DD\
  \ \u05E9\u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\u05D9 \u05EA\
  \u05E6\u05D5\u05E8\u05D4, \u05D9\u05D9\u05E6\u05D5\u05D0 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05D5\u05D4\u05E2\u05D1\u05E8\u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
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
