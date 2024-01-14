---
title:                "Kotlin: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

פעולות רגילות הם כלי חזק ומועיל בעבודה עם מחרוזות בקוד. הם מאפשרים לנו לחפש, לזהות ולבצע שינויים במחרוזות בצורה מאוד כללית ומודולרית. השתמשות בפעולות רגילות מקלה על הפיתוח ומקנה לנו יכולת גמישות רבה לעבוד עם מחרוזות.

## איך להשתמש

תחילה נצטרך להוריד את הספריית של פעולות הרגילות בקוד. בקוד קוטלין ישנם כמה שיטות להוספת ספרייה זו, בעזרת Maven או Gradle. לאחר מכן, נוסיף את הויתור \"import java.util.regex.*\" כדי להשתמש בפעולות הרגילות בקוד. ניתן להשתמש במתודות כגון match, find ו-replace על מנת לבצע חיפוש, זיהוי ושינויים במחרוזות בצורה גמישה וחזקה.

```Kotlin
val pattern = Regex("hello")
val result = pattern.matchEntire("hello world") 
// result = MatchResult

val sentence = "I love Kotlin"
val newSentence = Regex("love").replace(sentence, "like")
// newSentence = "I like Kotlin"
```

## העמקה

פעולות רגילות מציעות כמה טכניקות מתקדמות לעיבוד מחרוזות. למשל, ניתן להשתמש בעזרת Grupos (קבוצות) כדי לחלק את המחרוזת לחלקים ולגשת אליהם בנפרד. גם בעזרת Positive Lookahead ו-Negative Lookahead ניתן לבצע חיפושים מתקדמים על בסיס תנאים שונים. כמו כן, ניתן להשתמש בשילוב עם תנאי הסדרה על מנת לבצע בדיקות של סדרת תווים בלתי חוקיים. על מנת ללמוד עוד על הטכניקות המתקדמות של פעולות רגילות, ניתן לעיין במקורות התיעוד הרשמיים.

## ראה גם

- [מדריך לפעולות רגילות בקוטלין](https://kotlinlang.org/docs/regular-expressions.html