---
title:                "עבודה עם JSON"
aliases: - /he/java/working-with-json.md
date:                  2024-02-03T19:23:52.278802-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
