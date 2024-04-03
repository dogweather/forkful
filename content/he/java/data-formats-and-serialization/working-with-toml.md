---
date: 2024-01-26 04:23:33.265105-07:00
description: "TOML \u05D4\u05D5\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\
  \u05EA \u05E9\u05DC Tom's Obvious, Minimal Language. \u05D6\u05D4\u05D5 \u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E1\u05D9\u05D3\u05D5\u05E8 \u05D3\u05D0\u05D8\u05D4 \u05D4\
  \u05DE\u05E9\u05DE\u05E9 \u05DC\u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\
  \u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D5 \u05DE\u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05D5\
  \u05D0 \u05E7\u05DC \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4, \u05E7\u05DC \u05DC\u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D5\u05DE\u05DE\u05D5\u05E4\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.170641-06:00'
model: gpt-4-0125-preview
summary: "TOML \u05D4\u05D5\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\
  \u05EA \u05E9\u05DC Tom's Obvious, Minimal Language."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
