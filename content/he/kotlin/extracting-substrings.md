---
title:                "Kotlin: חילוץ תת-מחרוזות"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

כתיבת תוכניות בשפת קוטלין הולכת והייחודיות שלה מאפשרות עיבוד נתונים בצורה מהירה ויעילה. אחת מהטכניקות הנפוצות לעיבוד נתונים היא חיתוך מחרוזות לתת מחרוזות קטנות יותר. במאמר הזה נלמד כיצד לבצע חיתוך מחרוזות בקוטלין ונבין את ההוראות העמוקות מאחורי הטכניקה הזו.

## איך לעשות

חיתוך מחרוזות בקוטלין נעשה על ידי שימוש בפונקציה `substring(startIndex, endIndex)` שמקבלת שני פרמטרים: `startIndex` שזה האינדקס של התו הראשון שברצוננו לחתוך ו-`endIndex` שזה האינדקס של התו האחרון שברצוננו לחתוך. המילה המפותלת מעין הפס כדי להבהיר שאף לא תילמדם הפונקציה `startIncluded`, התוצאות האפשריות יכולות להיות ממש מונחים כדרך הבהירה. ננסה מה שנרצה כדי להתאים למחרוזות גם בחישוב:

```Kotlin
val str = "Hello World"
val substr1 = str.substring(0, 5)  // Output: "Hello"
val substr2 = str.substring(6)     // Output: "World"
```

כפי שאתם רואים, כאשר אנו נותנים רק `startIndex`, אנו מתחילים את החיתוך מהאינדקס הנתון ועד לסוף המחרוזת המקורית. הגדרת אף האינדקס האחרון שברצונכם מעלה את הכלל הכללי.

הנה כמה דוגמאות נוספות:

```Kotlin
val str = "This is a sentence."
val substr1 = str.substring(5, 7)  // Output: "is"
val substr2 = str.substring(8, 11) // Output: "a"
val substr3 = str.substring(12)    // Output: "sentence."
```

## כרפיה עמוקה

עכשיו שאתם מבינים את היסודות של חיתוך מחרוזות בקוטלין, נשטוף עיוור למה זה טוב. הנה כמה יתרונות של חיתוך מחרוזות:

- יעילות: