---
title:                "Java: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## על מה?

ימיימל (YAML) הוא שפה פשוטה להבנה שמשמשת להגדרת מבנה נתונים מהוקשרים במחשב. זו נפוצה בפרוייקט הרכבה האפליקציה ומשמשת גם להעברת מידע בין יישומים שונים. בתור מתכנת ג'אווה, עלייך למרות בפנימה עם הנתונים הללו וכך נוצר שירותים תקינים שניתן לנהל בקלות.

## איך לעשות?

עבודה עם ימל בג'אווה פשוטה ונוחה למדי. ניתן לבצע קריאה וכתיבה של נתונים בפורמט ימל על ידי כתיבת קוד פשוט תוך שימוש בספרייה של הימל. הנה דוגמה לקריאה והדפסת קובץ ימל בג'אווה:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;

public class YAMLReader {

    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = YAMLReader.class.getResourceAsStream("data.yaml");
        Map<String, Object> data = (Map<String, Object>) yaml.load(inputStream);
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            System.out.println("Key: " + entry.getKey() + "\nValue: " + entry.getValue());
        }
    }

}
```

כתובת הקובץ "data.yaml" יכולה להיות כל שם שנבחר ובמידה והקובץ נמצא בתקיית הפרוייקט, ניתן לקרוא אותו באמצעות פקודה כזו.

הנתונים יוטלו בסוג של מפתח-ערך, ולכן נוכל להתמודד עם כל סוגי הנתונים, כגון מחרוזות, מספרים, בוליאניים וכו'. התוצאה של הקוד שלעיל תהיה:

```yaml
Key: name
Value: John
Key: age
Value: 30
Key: married
Value: true
```

ניתן גם ליצור קובץ ימל בפורמט ימל תוך שימוש בספריית הימל. הנה דוגמה לכך:

```java
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class YAMLWriter {

    public static void main(String[] args) {
        Map<String, Object> data = new HashMap<>();
        data.put("name", "John");
        data.put("age", 30);
        data.put("married", true);
        DumperOptions options = new Dumper