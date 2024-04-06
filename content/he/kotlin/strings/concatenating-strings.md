---
date: 2024-01-20 17:35:42.368943-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D3\u05D5\
  \u05D2\u05DE\u05D4 \u05D4\u05D6\u05D5 \u05E8\u05D0\u05D9\u05E0\u05D5 \u05D0\u05D9\
  \u05DA \u05E4\u05E9\u05D5\u05D8 \u05E0\u05D9\u05EA\u05DF \u05DC\u05E9\u05E8\u05E9\
  \u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05E7\u05D5\u05D8\u05DC\
  \u05D9\u05DF."
lastmod: '2024-04-05T21:53:40.465588-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D3\u05D5\u05D2\u05DE\u05D4 \u05D4\u05D6\u05D5 \u05E8\u05D0\u05D9\
  \u05E0\u05D5 \u05D0\u05D9\u05DA \u05E4\u05E9\u05D5\u05D8 \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05E9\u05E8\u05E9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\
  \u05E7\u05D5\u05D8\u05DC\u05D9\u05DF."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
```Kotlin
fun main() {
    val hello = "שלום"
    val world = "עולם"
    val concatenated = hello + " " + world + "!"
    println(concatenated) // ידפיס: שלום עולם!
}
```
    
בדוגמה הזו ראינו איך פשוט ניתן לשרשר מחרוזות בקוטלין.

```Kotlin
fun main() {
    val words = listOf("שלום", "עולם", "מתכנתים")
    val sentence = words.joinToString(separator = " ") { it }
    println(sentence) // ידפיס: שלום עולם מתכנתים
}
```
הדוגמה הזו מראה דרך נוספת לשרשר רשימה של מחרוזות בקוטלין.

## ניתוח עמוק
היסטורית, שרשור מחרוזות היה אחד הפעולות הבסיסיות בתכנות, דרך אמתחת שפות קידומות כמו C וJavה. בקוטלין, שרשור נעשה גם עם פלוס `+` וגם עם פונקציות כמו `joinToString`. יש אלטרנטיבות נוספות, כמו string templates להכנסה של משתנים בקלות בתוך מחרוזת: `"$hello $world!"`. שרשור יכול להיות פחות יעיל בכמות גדולה כיוון שמחרוזות בקוטלין הן immutable, ולכן לכל שרשור נוצרת מחרוזת חדשה. למזלנו, קומפיילרים מודרניים כמו של קוטלין, מתאימים את הביצועים בהתאם.

## ראה גם
- [Kotlin Documentation: String templates](https://kotlinlang.org/docs/basic-types.html#string-templates)
- [Optimizing String concatenation in Kotlin Bytecode](https://proandroiddev.com/optimizing-string-concatenation-in-kotlin-bytecode-2be0305b3f7)
