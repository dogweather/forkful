---
title:                "הגדלת אותיות במחרוזת"
aliases:
- he/java/capitalizing-a-string.md
date:                  2024-02-03T19:06:30.381460-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות רישיות כוללת שינוי של האות הראשונה של כל מילה במחרוזת לאות גדולה תוך כדי שמירה על השאר באותיות קטנות. משימת עיבוד מחרוזות זו נפוצה ושימושית לעיצוב טקסט ביישומים, כגון הכנת שמות משתמש או כותרות לתצוגה לפי מוסכמה או נכונות דקדוקית.

## איך לעשות זאת:
ספריית הסטנדרט של Java לא מספקת שיטה ישירה להפוך מחרוזות שלמות לאותיות רישיות בבת אחת, אך ניתן להשיג זאת באמצעות שילוב של מתודות מובנות. לצרכים מתקדמים יותר, ספריות של צד שלישי כמו Apache Commons Lang מציעות פתרונות ישירים.

### באמצעות מתודות מובנות של Java
להפיכת מחרוזת לאותיות רישיות ללא ספריות חיצוניות, ניתן לפצל את המחרוזת למילים, להפוך את האות הראשונה של כל אחת לאות גדולה ולאחר מכן לחבר אותן שוב. הנה גישה פשוטה:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // פלט: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

קטע קוד זה ממיר את כל המחרוזת לאותיות קטנות, ולאחר מכן עובר על כל תו ותו, ומפוך את האות הראשונה של כל מילה לאות גדולה. הוא רואה ברווחים, נקודות וגרשים כמפרידי מילים.

### באמצעות Apache Commons Lang

הספרייה Apache Commons Lang מספקת פתרון יותר אלגנטי עם המתודה `WordUtils.capitalizeFully()`, אשר מתמודדת עם מגוון מקרי קצה ומפרידים בשבילך:

```java
// הוסף תלות: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // פלט: "Hello, World!"
    }
}
```

כדי להשתמש במתודה זו, יש להוסיף את הספרייה Apache Commons Lang לפרויקט שלך. מתודת הספרייה לא רק מפוך את האות הראשונה של כל מילה לאות גדולה אלא גם ממירה את שאר האותיות בכל מילה לאותיות קטנות, ובכך מבטיחה דפוס הופכה עקבי ברחבי המחרוזת.
