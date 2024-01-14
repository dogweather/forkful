---
title:                "Kotlin: הפכו מחרוזת לאותיות רישיות"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

מחרוזת קיימת בכמעט כל תכנית בקוד ולעיתים קרות שאנו רוצים להוסיף את כל האותיות הראשונות שלה באותיות גדולות. לדוגמה, ייתכן שנרצה להפוך את המשפט "זו היא משפט חשוב" ל "זו היא משפט חשוב בכל האותיות הראשונות גדולות". לכן, החלטנו לכתוב בלוג זה על איך לבצע את הפעולה הזאת באמצעות קוד Kotlin.

## איך לעשות זאת

הנה שתי דרכים להפעיל את העיבוד של מחרוזת עם אותיות גדולות בשפת Kotlin:

```Kotlin
fun capitalizeString(str: String): String {
    // ראשית, נעשה מחרוזת קטנה כדי שלא נשנה את המשפט המקורי
    val smallStr = str.toLowerCase()
    
    // ניקח את המילים במחרוזת ונבדוק את האות הראשונה של כל מילה
    // אם היא אות קטנה, נחליף אותה באות גדולה
    return smallStr.split(" ").joinToString(" ") { word ->
        word.replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(
                Locale.getDefault()
            ) else it.toString()
        }
    }
}

println(capitalizeString("זו היא משפט חשוב"))
// Output: זו היא משפט חשוב

println(capitalizeString("this is an important sentence"))
// Output: This Is An Important Sentence
```

אפשר לראות שהפעולה עובדת גם עם מחרוזת ארוכה יותר וגם עם מחרוזת מרובעת כמו "זהו משפט חשוב לשני אנשים".

הדרך השניה היא להשתמש בפונקציה פנימית קיימת בשפת Kotlin שקוראת "replaceFirstChar", שגם מבצעת את הפעולה הזאת:

```Kotlin
fun capitalizeString(str: String): String {
    return str.replaceFirstChar {
        if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
    }
}

println(capitalizeString("זהו משפט חשוב לשני אנשים"))
// Output: זהו משפט חשוב לשני אנשים
```

בשתי הדרכים, אנו מצליחים להכניס את המשפט המקורי עם אותיות גדולות.

## חפירה עמוקה

בגרסה