---
title:                "השוואת שתי תאריכים"
html_title:           "Kotlin: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

כמעט בכל יישום אנחנו מתקשים לטפל בתאריכים. לדוגמה, נרצה להציג למשתמש מידע על תעריפים על פי תאריך הסעה שנבחר ותאריך הסעה הבאה. כיצד נבדוק אם הטווח נבחר לאחר התאריך הנוכחי או שכבר עבר?

## כיצד

כדי להשוות שני תאריכים ב-Kotlin, נשתמש בפונקציות כמו השוואות, חיסכון בלוקים, פיענוח תאריך וניסיון.

```Kotlin
// פיענוח תאריך מתאריך נתון
val firstDate = LocalDate.parse("2021-09-01")
val secondDate = LocalDate.now()

// השוואה בין שני תאריכים לפי טווח
if(firstDate.isAfter(secondDate)) {
    // פעולות לתאריך אחר הנוכחי
} else if(firstDate.isBefore(secondDate)) {
    // פעולות לתאריך קודם לנוכחי
} else {
    // פעולות לתאריך הנוכחי
}
```

## חקירה מעמיקה

מקרים רבים עלולים להיות מורכבים יותר, לדוגמה כאשר חשוב להשוות גם את השעה בתאריך ולטפל באזורי זמן שונים.

בנוסף, כדי להגיע לתוצאות מדויקות, חשוב לקחת בחשבון גם תנאים כגון כמות הימים בחודש, שנה מעוברת ואפילו שינויים ביומני החורף והקיץ.

בסופו של דבר, ככל שנלמד יותר על החשיבה סביב תאריכים, ניתן להכין קוד יעיל יותר שיספק תוצאות מדויקות ומהירות יותר.

## ראה גם

למידע נוסף על כיצד להשוות תאריכים ב-Kotlin, היכנס לכתבה הבאה:

- [טיפים לעבודה עם תאריכים ב-Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date.html)
- [ניסיון לשווא שני תאריכים ב-Kotlin](https://kotlinlang.org/docs/comparisons.html#working-with-dates)