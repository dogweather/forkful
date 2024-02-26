---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:52.278802-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (\u05EA\u05E1\u05D3\
  \u05D9\u05E8 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05E9\u05DC\
  \ JavaScript) \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\u05D8\u05E4\u05DC \u05D1\
  \u05EA\u05E1\u05D3\u05D9\u05E8 \u05D4\u05D7\u05DC\u05D9\u05E4\u05D9\u05DF \u05D4\
  \u05E7\u05DC \u05D4\u05D6\u05D4 \u05D1\u05EA\u05D5\u05DA \u05D4\u05D9\u05D9\u05E9\
  \u05D5\u05DE\u05D9\u05DD \u05E9\u05DC\u05DA \u05D1-Java. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD \u05D1-JSON \u05DB\u05D3\
  \u05D9 \u05DC\u05E1\u05E8\u05D9\u05D0\u05DC\u05D6 \u05D5\u05DC\u05E9\u05D3\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-02-25T18:49:37.402769-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (\u05EA\u05E1\u05D3\u05D9\
  \u05E8 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05E9\u05DC JavaScript)\
  \ \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\u05D8\u05E4\u05DC \u05D1\u05EA\u05E1\
  \u05D3\u05D9\u05E8 \u05D4\u05D7\u05DC\u05D9\u05E4\u05D9\u05DF \u05D4\u05E7\u05DC\
  \ \u05D4\u05D6\u05D4 \u05D1\u05EA\u05D5\u05DA \u05D4\u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD \u05E9\u05DC\u05DA \u05D1-Java. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05D1\u05D5\u05D7\u05E8\u05D9\u05DD \u05D1-JSON \u05DB\u05D3\u05D9 \u05DC\
  \u05E1\u05E8\u05D9\u05D0\u05DC\u05D6 \u05D5\u05DC\u05E9\u05D3\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON (תסדיר אובייקטים של JavaScript) פירושה לטפל בתסדיר החליפין הקל הזה בתוך היישומים שלך ב-Java. מתכנתים בוחרים ב-JSON כדי לסריאלז ולשדר נתונים מובנים דרך רשת וגם קל להגדיר ולאחסן נתונים מכיוון שהוא קריא לבן אדם ובלתי תלוי שפה.

## איך ל:
בואו נגלגל שרוולים ונתחיל לתכנת עם JSON ב-Java.

ראשית, תצטרכו ספריית עיבוד JSON כמו `Jackson` או `Google Gson`. כאן נשתמש ב-`Jackson`, לכן הוסיפו תלות זו ל-`pom.xml` שלכם:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

עכשיו, בואו נסריאלז (נכתוב) אובייקט ג'אווה פשוט ל-JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

הפלט יהיה:

```json
{"name":"Alex","age":30}
```

עכשיו, לדיסריאליזציה (קריאה) של JSON חזרה לאובייקט ג'אווה:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " בן " + person.age + " שנה.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

הפלט יהיה:

```
Alex בן 30 שנה.
```

## צלילה עמוקה
הפשטות והיעילות של JSON הפכו אותו לתקן הדה-פקטו להחלפת נתונים ברשת, והדיחו את XML מעל כיסאו. המוצג בראשית שנות ה-2000, JSON נגזר מ-JavaScript אך כיום נתמך ברוב השפות.

אלטרנטיבות ל-JSON כוללות XML, שהוא יותר מפורט, ותבניות בינאריות כמו Protocol Buffers או MessagePack, שהן פחות קריאות לבן אדם אך יעילות יותר בגודל ובמהירות. לכל אחת מהן יש מקרי שימוש; הבחירה תלויה בצרכי הנתונים הספציפיים ובהקשר שלכם.

ב-Java, מעבר ל-`Jackson` ו-`Gson`, יש לנו גם את `JsonB` ו-`org.json` כספריות נוספות לטיפול ב-JSON. Jackson מציע עיבוד מבוסס זרם וידוע במהירותו, בעוד ש-Gson מפורסם בקלות השימוש שלו. JsonB הוא חלק מ-Jakarta EE, ומציע גישה יותר מתקנתית.

כשמיישמים את JSON, זכרו לטפל היטב בחריגות שלכם - הקוד שלכם צריך להיות עמיד נגד קלטים שגויים. כמו כן, שקלו את ההשלכות הביטחוניות של קשירת נתונים אוטומטית - תמיד אמתו את קלטיכם!

## ראו גם
- [פרויקט Jackson](https://github.com/FasterXML/jackson)
- [הפרויקט Gson](https://github.com/google/gson)
- [מפרט JSON](https://www.json.org/json-en.html)
- [מפרט JsonB](https://jakarta.ee/specifications/jsonb/)
