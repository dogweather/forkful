---
title:                "Kotlin: חיפוש והחלפת טקסטים"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מדוע

למה בכלל צריך להתעסק בחיפוש והחלפת טקסט? כי הם כלי עיקריים לעיבוד טקסט ומאפשרים לנו לבצע פעולות מסוימות על טקסטים בקלות ובמהירות.

## כיצד לעשות זאת

כאשר אנו עובדים עם קוד, ייתכן שנתקל בצורך לחפש ולהחליף טקסט בכמה מקומות בקוד. בשפת קוטלין, ישנם מספר דרכים לבצע פעולת חיפוש והחלפה של טקסט.

תחילה, נצטרך להגדיר מחרוזת של הטקסט שנרצה לחפש ולהחליף. לדוגמה:

```Kotlin
val oldText = "Hello"
val newText = "Bonjour"
```

כעת, נוכל להשתמש בפעולת החיפוש והחלפה כדי לחלוף את הטקסט הישן בחדש בכל מקום בקוד. ניתן לעשות זאת באמצעות פיקוד ה-[replaceFirst] או ה-[replace] כפי שמסופק בדוגמה הבאה:

```Kotlin
val myString = "Hello World!"
val newString = myString.replaceFirst(oldText, newText)
println(newString) // Bonjour World!
```

בנוסף, ניתן להשתמש באופרטור [in] כדי לבדוק אם טקסט מסוים נמצא בתוך מחרוזת. לדוגמה:

```Kotlin
val myString = "Hello World!"
if(oldText in myString) {
    println("Old text found!")
}
else {
    println("Old text not found.")
}
```

בתוך פונקציית ה-[replace] ניתן להשתמש בביטוי רגולרי כדי לחלוף את הטקסט בצורה יותר גמישה. לדוגמה:

```Kotlin
val myString = "Today is a sunny day."
val newString = myString.replace(Regex("[Ss]unny"), "cloudy")
println(newString) // Today is a cloudy day.
```

כדי לחלק את הטקסט לפי תבנית מסוימת, ניתן להשתמש בפונקציית [split] ולהגדיר את התבנית כמחרוזת או ביטוי רגולרי. לדוגמה:

```Kotlin
val myString = "One,Two,Three,Four,Five"
val myList = myString.split(",")
println(myList) // [One, Two, Three, Four, Five]
```

## ניר