---
title:                "עבודה עם yaml"
html_title:           "Kotlin: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם YAML היא פשוט תהליך שבו נוצר קובץ טקסט שמכיל נתונים מסוג מפתח עץ ותווי קו. תכונתה המשותפת היא קריאות, כך שמנמך את כמות הזמן שנדרש כדי לקרוא את הקובץ. הסיבה שמובילה מתכנתים לעבוד עם YAML היא כי היא מאפשרת ליצור קבצים קלים לקריאה ולעיבוד.

## איך לבצע:
```Kotlin
val yaml = """ 
    name: John Smith 
    age: 30 
    isMarried: true 
""".trimIndent() 
```
כדי ליצור קובץ YAML, מתחילים עם תווים כפולים ומגדירים את השדות והערכים בצורה של מיפתח עץ בין התווים. ניתן להכניס נתונים כמו סטרינגים, מספרים, בוליאניים, ועוד. לאחר התקנת הסיבובים המתאימים, התוכן של הקובץ YAML יופיע במופע המשוייך.

## מעמקים:
YAML נוצרה בשנת 2001 ונחשבת לשפת מממודרגת. ישנן אלטרנטיבות משנה כמו XML ו-JSON, אך YAML נחשבת לפשוטה יותר משתי האפשרויות האחרות. הפימור הראשון לקובץ YAML היה של קצת טיפש, כך שכדאי להשתמש בעצמך כאשר אתה מוכן לעבוד על קובץ חדש.

## ראה גם:
למידע נוסף על YAML, ניתן לבחון את הקישורים הבאים:
- [הגדרת YAML](https://yaml.org/spec/1.2/spec.html)
- [מדריך לשימוש ב-YAML בקוד (באנגלית)](https://www.baeldung.com/kotlin/yaml)
- [דוגמאות של קבצי YAML](https://github.com/chbrown/magic-strings/blob/master/lib/magic_strings.py)