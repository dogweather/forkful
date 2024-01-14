---
title:                "Kotlin: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה

המרת מחרוזת לאותיות קטנות היא פעולה נפוצה בתכנות בכל שפת תכנות. היא מאפשרת לנו לשנות את המחרוזת המקורית לגרסה בתובנות נמוכות יותר, מה שיכול להיות שימושי במגוון מקרים, כגון השוואת טקסט או ניהול נתונים.

# איך לעשות זאת

אם אתם מתכנתים בשפת תכנות Kotlin, ישנן שתי דרכים להמיר מחרוזת לאותיות קטנות. הראשונה היא באמצעות הפונקציה "toLowerCase()" והשנייה היא על ידי שימוש באופרטור "!!.toLowerCase()". בשתי המקרים, המחרוזת המקורית תוחזר בגרסת אותיות קטנות.

```Kotlin
val name = "JOHN"
println(name.toLowerCase()) // תוצאה: john
println(name!!.toLowerCase()) // תוצאה: john
```

כמו כן, ניתן להשתמש בלולאה כדי להמיר מחרוזת צרורה לאותיות קטנות. בדוגמה זו, אנו יוצרים משתנה חדש ומשתמשים במתודת "toLowerCase()" על ידי הפקדה שלה לכל תו במחרוזת המקורית.

```Kotlin
val name = "JOHN"
var newName = ""
for (char in name){
    newName += char.toLowerCase()
}
println(newName) // תוצאה: john
```

# עיון מעמיק

ברצוני להציג לכם עוד אפשרות עבור מרת מחרוזות לאותיות קטנות. במקום להשתמש במתודות ואופרטורים מובנים, ניתן להשתמש בפונקציה עזר נלווה שנקראת "toLowercase()". הפונקציה תעבוד באותו האופן כמו שימוש בפונקציה "toLowerCase()", אך היא מאפשרת לנו להכליל את הפעולה לכל סוגי הנתונים.

```Kotlin
fun String.toLowercase(): String {
    var result = ""
    for (char in this){
        if(char.isUpperCase()){
            result += (char.toInt() + 32).toChar()
        } else {
            result += char
        }
    }
    return result
}

val name = "JOHN"
println(name.toLowercase()) // תוצאה: john
```

# ראו גם

- [מ