---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים קל לקריאה, משמש להגדרת תצורה. תוכניות רבות משתמשות ב-YAML כיוון שהוא אינטואיטיבי וקל לעריכה על ידי בני אדם.

## איך לעשות:
כדי לעבוד עם YAML ב-Java צריך להשתמש בספריה כמו `snakeyaml`. הדוגמה הבאה מראה כיצד לקרוא ולכתוב קובצי YAML.

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.io.FileWriter;
import java.util.Map;

public class YamlExample {

    public static void main(String[] args) {
        Yaml yaml = new Yaml();

        // קריאה מ-YAML
        InputStream inputStream = YamlExample.class
            .getClassLoader()
            .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
        
        // כתיבה ל-YAML
        String outputYaml = yaml.dump(data);
        try (FileWriter writer = new FileWriter("output.yaml")) {
            writer.write(outputYaml);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

פלט דוגמה (מהדפסת המפה):

```
{server=apache, database=mysql, version=1.0}
```

## צלילה לעומק:
YAML נוסד ב-2001 ומכוון להיות ידידותי לקודר. ישנן חלופות כמו JSON וXML, אך YAML נותר פופולרי בזכות הקריאות והפשטות שלו. בשפת Java, ניתן להתממשק ל-YAML באמצעות ספריות כמו `snakeyaml` שפותחה על ידי אסף הידיד.

## ראה גם:
- המדריך הרשמי של `snakeyaml`: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- YAML 1.2 מפרט רשמי: https://yaml.org/spec/1.2/spec.html
- השוואת YAML ל-JSON ו-XML: https://www.json2yaml.com/
