---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:30.381460-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \ \u05E9\u05DC Java \u05DC\u05D0 \u05DE\u05E1\u05E4\u05E7\u05EA \u05E9\u05D9\u05D8\
  \u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05E9\u05DC\u05DE\u05D5\u05EA \u05DC\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA \u05D1\u05D1\u05EA\
  \ \u05D0\u05D7\u05EA, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D9\
  \u05D2 \u05D6\u05D0\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\
  \u05DC\u05D5\u05D1 \u05E9\u05DC \u05DE\u05EA\u05D5\u05D3\u05D5\u05EA \u05DE\u05D5\
  \u05D1\u05E0\u05D5\u05EA.\u2026"
lastmod: '2024-03-13T22:44:39.102641-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC Java \u05DC\u05D0 \u05DE\u05E1\u05E4\u05E7\u05EA \u05E9\u05D9\
  \u05D8\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E9\u05DC\u05DE\u05D5\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA \u05D1\u05D1\
  \u05EA \u05D0\u05D7\u05EA, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\
  \u05D9\u05D2 \u05D6\u05D0\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\
  \u05D9\u05DC\u05D5\u05D1 \u05E9\u05DC \u05DE\u05EA\u05D5\u05D3\u05D5\u05EA \u05DE\
  \u05D5\u05D1\u05E0\u05D5\u05EA."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

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
