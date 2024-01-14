---
title:                "Java: להשוות שתי תאריכים"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

למה שלפעמים נרצה להשוות בין שתי תאריכים בתוכנת Java? לדוגמה, במיקרים של מערכת ניהול מלאי כאשר נרצה לבדוק האם פריטים נמצאים במלאי עדכני או כשרוצים לתאריך מסוים התקנות תוכנה. כללי למידה זהו הוא חשוב מאוד במתכנתים ונישמרת עדי עצמנו.

## איך לעשות

כאן נראה דוג' תוספת של ל' ירדך 'כדי לבדוק את התאריכים בשנת 2021.

```java
import java.time.LocalDate;

public class DateComparator {
    public static void main(String[] args) {

        // פנינה את התאריך שאני רוצה לבדוק
        LocalDate date1 = LocalDate.of(2021, 4, 1);
        LocalDate date2 = LocalDate.of(2021, 5, 1);

        // לעשות השוואה בין התאריכים
        int result = date1.compareTo(date2);

        // הודעה בהתאם לתוצאה
        if (result < 0) {
            System.out.println("תאריך בעתיד הוא");
        } else if (result == 0) {
            System.out.println("התאריך זהה לתאריך הנוכחי");
        } else {
            System.out.println("תאריך בעבר הוא");
        }

    }
}
```

הפלט של הקוד זה מציג את התאריכים ואת הקווים הודעה לפי תוצאת ההשוואה:

```plaintext
תאריך בעבר הוא
```

בפסקה הבאה, אנו נוכל לראות את דוקונטציית ה 'ללחמם מחביאת שלא היוחי' בתוכנת Java ספירת ספרה בעולם עדכני. יש לדעת, ספירת ספרה בתוכנת Java לא מתקנת את קויתיים התוכנה הלאסיינסים לכתוב בשפת הוסקה כי אני לעת יריכה. ניתן להשתמש בפרמוטיות צבע של Java כדי להראות דוג' קויתיים חלקים, ואף נוכל להשתמש בכלים נלווים כדי לקבוע ולהשוות בבינוי של הקוד.

## חפירת עמוקה

מתאריכים בתוכנית Java זנב תכנית Java בתוכנית ת,תכנית של א'כספור