---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיבול ראשיות במחרוזת זהו התהליך שבו אתה מעלה את אותיות המחרוזת לאותיות רישיות. תוכניתנים עושים זאת לעתים קרובות כדי לאחד פורמטים עבור תצוגה אחידה או לצורך קליטות טובה יותר.

## איך לעשות:
הנה כמה דוגמאות קוד:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String original = "שלום עולם!";
        String capitalized = original.toUpperCase();
        System.out.println(capitalized); // שולם עולם!
    }
}
```

פלט לדוגמה:

```
שלום עולם!
```

## צלילה עמוקה:
כאשר מדברים על קיבול ראשיות, האפשרות הפשוטה והישירה של שימוש ב`toUpperCase()` ב-Java היא רק עלה הקרחון. לעתים קרובות יש להתחשב בשפה ובתרבות מכיוון שלא כל השפות משתמשות בראשיות באותה האופן. בהיסטוריה, שימוש בראשיות היה נפוץ במקורות קוד שכונו בלשון עתית לסימון קבועים, כיתות, או אובייקטים בקוד שצריך להיות בולט וקריא.

בנוסף קיימות מספר אלטרנטיבות למתודה `toUpperCase()`ת כמו השימוש ב`capitalize()` מApache Commons Lang, שמאפשר להגדיל רק את האות הראשונה של כל מילה במחרוזת. לדוגמה:

```java
import org.apache.commons.lang3.StringUtils;

public class CapitalizeStringExample {
    public static void main(String[] args) {
        String original = "שלום עולם!";
        String capitalized = StringUtils.capitalize(original);
        System.out.println(capitalized); // שלום עולם!
    }
}
```

פרטים טכניים כוללים את העובדה ש`toUpperCase()` יכול לתלות ב-Locale, ולכן עשוי להחזיר תוצאות שונות עבור תווים שאינם ASCII לידי תצורות שפה ואזור שונות.

זכרו: ביצוע קיבול ראשיות צריך להתבצע במודעות תרבותית ולשקול את השפה והמבנה הגרמטי של הטקסט שאתם עובדים איתו.

## ראה גם:
- [מסמכי Java String toUpperCase](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [Locale תמיכה ב-Java בעבודה עם טקסטים](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
