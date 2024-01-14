---
title:    "Kotlin: חיפוש והחלפת טקסט"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה:

מצוין יחתוב קוד בקובץ גדול שלא ברור לב. קריאה כבדה פרושה של רק עו"ה ובאופן כללי נכון העברה השפות עשו

## איך לעשות:

כדי לחלוף טקסט בסמן הקווים המכפול "==" על המילה `replace` הוא יחתובית הטקסט "ו" כך:

```Kotlin
val originalText = "זהו רקפידה"
val modifiedText = originalText.replace("רק", "שליטה")
println(modifiedText)
```

פלט:

שליטהפאנס זהו רקפידה

עוד אפשרות לחלוף טקסט היא להשתמש בפעולת `replaceAll()` ולציין את המילה החדשה כדי לחלוף את כל המופעים של המילה המצוייה:

```Kotlin
val originalText = "זהו רקפידה רקפידה רקפידה"
val modifiedText = originalText.replaceAll("רק", "שליטה")
println(modifiedText)
```

פלט:

שליטהפאנס זהו שליטהפאנס שליטהפאנס שליטהפאנס

## חפירה עמוקה:

לחילוף טקסט ישנם מתודות נוספות כמו `replaceFirst()` שמאפשרת רק להחליף את המופע הראשון של המילה המצוייה בטקסט. ניתן גם להשתמש בפטרנים כדי לחפש את המילה המצוייה בטקסט. למדריך מפורט יותר על כיצד לחלוף טקסט עבור פטרנים, ראו [מדריך קודלבוג](https://www.codeleoblog.com/kotlin-string-methods-text-replacement-patterns).

## ראו גם:

- [המדריך המלא של קודלבוג לחילוף טקסט בקוד Kotlin](https://www.codeleoblog.com/kotlin-string-methods-text-replacement-patterns)
- [היכן ניתן להשתמש בחליפת טקסט בפרויקטי Kotlin שלכם](https://kotlinlang.org/docs/string.html#string-replacement)
- [הצגת מתודות מובנות לשינוי טקסט ולחילוף בקוד Kotlin](https://www.tutorialkart.com/kotlin-strings/kotlin-string-replace-method-functions/)