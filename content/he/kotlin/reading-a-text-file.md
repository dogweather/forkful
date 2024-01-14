---
title:    "Kotlin: קריאת קובץ טקסט"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# למה

קריאת קובץ טקסט היא חלק חשוב בתכנות ב-Kotlin ומאפשרת עיבוד וניתוח של מידע מורכב בפשטות ויעילות. אם אתם מתעניינים בתכנות ורוצים ללמוד כיצד לקרוא קובץ טקסט ב-Kotlin, אתם במקום הנכון!

# איך לעשות זאת

כדי לקרוא קובץ טקסט ב-Kotlin, נוכל להשתמש בפונקציה המובנית `readText()` שתקרא את התוכן של הקובץ ותחזיר אותו כמחרוזת. לדוגמה, אם נרצה לקרוא קובץ טקסט בשם "example.txt" שנמצא באותה התיקייה כמו הקוד שלנו, נוכל לכתוב:

```Kotlin
val text = readText("example.txt")
println(text)
```

הפונקציה `readText()` מצפה לקבל כפרמטר את שם הקובץ שנרצה לקרוא ואת המחרוזת שתחזיר תופיע במשתנה `text` ונוכל להשתמש בה לאחר מכן לפי הצורך.

# העמקה

למרבה המזל, ב-Kotlin ישנן פונקציות נוספות שניתן להשתמש בהן כדי לקרוא ולעבד קבצי טקסט. לדוגמה, נוכל להשתמש בפונקציה `bufferedReader()` על מנת לקרוא את הקובץ בצורה יעילה יותר. כמו כן, ניתן להשתמש במחלקת `File` על מנת לבצע פעולות יותר מתקדמות כמו קריאה שורה-שורה או ניתוח נתונים מבני המבנה של הקובץ.

# ראו גם

- [טפסים וטקסט ב-Kotlin](https://www.programiz.com/kotlin-programming/input-output)
- [כיצד לקרוא ולכתוב קבצי טקסט ב-Kotlin](https://www.geeksforgeeks.org/reading-and-writing-strings-to-a-file-in-kotlin/)
- [מדריך מקיף לקריאת וכתיבת קבצי טקסט ב-Kotlin](https://www.baeldung.com/kotlin-file)