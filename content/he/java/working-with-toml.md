---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:23:33.265105-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML הוא ראשי תיבות של Tom's Obvious, Minimal Language. זהו פורמט סידור דאטה המשמש לקבצי תצורה. מתכנתים משתמשים בו מכיוון שהוא קל לקריאה, קל לכתיבה וממופה יפה לטבלת האש.

## איך לעשות:
תזדקק לספריית ניתוח TOML. אני ממליץ על `toml4j`. הוסף אותה לפרויקט שלך כך:

```java
// הוסף את זה ל-build.gradle שלך
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

הנה איך מנתחים קובץ TOML:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("IP שרת: " + ip);
        System.out.println("פורט שרת: " + port);
    }
}
```

דוגמה לפלט:

```
IP שרת: 192.168.1.1
פורט שרת: 80
```

## צלילה עמוקה
TOML, שפותח על ידי תום פרסטון-ורנר, שותף-מייסד GitHub, שאף להיות פשוט יותר מ-XML ויותר מומצא מ-YAML. הגרסה האחרונה שלו, 1.0.0, ששוחררה ב-2021, מציעה סט של תכונות יציב.

אלטרנטיבות כמו JSON או YAML גם הן פופולריות. JSON מעולה להחלפת נתונים. YAML קריא יותר לאדם עבור תצורות מורכבות. חוזקו של TOML הוא בפשטותו ובשימושו בקהילת Rust.

בקשר ליישום, כאשר משתמשים ב-TOML עם ג'אווה, חשוב לזכור שהניתוח שתבחר משנה. מעבר ל-`toml4j`, יש כאלו שבוחרים ב-`jackson-dataformat-toml`. לכל אחד  יהיו ניואנסים, כמו טיפול בשגיאות או ביצועי ניתוח, אז בחר בהתאם לצרכי הפרויקט שלך.

## ראה גם
- מפרט TOML: https://toml.io/en/
- `toml4j` ב-GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
