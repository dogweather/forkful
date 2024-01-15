---
title:                "פלט תיקון בעיות"
html_title:           "Kotlin: פלט תיקון בעיות"
simple_title:         "פלט תיקון בעיות"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

אנשים רבים נוהגים להשתמש בפלט דיבאג מתוך צורך לנתח ולייעל את הקוד הקיים, לזהות באגים ולמצוא פתרונות לבעיות טכניות. הדפסת פלט דיבאג הינה כלי עזר מצוין לסנן ולעקוב אחרי המידע המופיע בקוד ולקבל מידע מעמיק ומדוייק לגבי פעולת התוכנית.

## איך לעשות זאת

כדי להדפיס פלט דיבאג בשפת קוטלין, אנו נשתמש בפונקציית `println()` או בפונקציית `print()` כדי להדפיס מחרוזת ללוג. יש לנו אפשרות להדפיס את המידע המופיע בתוך משתנים, כמו גם להדפיס ערכים ספציפיים או כל כלל אחר שנרצה לבדוק.

```Kotlin
val num1 = 5
val num2 = 10
println("The value of num1 is $num1 and the value of num2 is $num2")
```

פלט:

```
The value of num1 is 5 and the value of num2 is 10
```

ניתן גם להשתמש בפונקציית `println()` כדי להדפיס מידע על משתנים מסוימים בתוך אובייקטים. לדוגמה:

```Kotlin
class Person(val name: String, val age: Int)

val person = Person("John", 25)
println("Name: ${person.name}\nAge: ${person.age}")
```

פלט:

```
Name: John
Age: 25
```

עוד אחת האפשרויות המעניינות היא להשתמש בפונקציית `asssert()` כדי להדפיס מידע על תנאים או ערכים שנבדקים בזמן ריצת הקוד. לדוגמה:

```Kotlin
val num = 7
assert(num > 10) { "Num must be greater than 10!" }
println("The value of num is $num")
```

פלט:

```
Exception in thread "main" java.lang.AssertionError: assertion failed: Num must be greater than 10!
The value of num is 7
```

בנוסף, בשפת קוטלין ישנם כלים נוספים כמו `printStackTrace()` שמאפשרים להדפיס מידע נוסף על פתרון הבעיה וקריאה לפונקציות שונות כדי לבדוק מידע נוסף על הקוד.