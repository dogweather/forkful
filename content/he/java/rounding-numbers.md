---
title:                "עיגול מספרים"
date:                  2024-01-26T03:46:12.021799-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים משמעו התאמתם לדרגת דיוק מסוימת. מתכנתים עושים זאת כדי לפשט מספרים לקריאות, לעמוד בהגדרות מסוימות או כדי לוודא שהחישובים נכנסים בתוך גבולות מסוימים, כמו למנוע שגיאות דיוק בחישובים עם מספרים צפים.

## איך לעשות:
Java מציעה מספר דרכים לעגל מספרים. הנה הדגמה מהירה עם `Math.round()`, `BigDecimal`, ו-`DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // באמצעות Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // פלט: 123

        // באמצעות BigDecimal לשליטה רבה יותר
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // פלט: 123.46

        // באמצעות DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // פלט: 123.46
    }
}
```

## צלילה עמוקה
בהיסטוריה, עיגול מספרים היה חיוני לחישובים אנלוגיים והועבר לחישוב דיגיטלי לצורך יעילות ודיוק. שגיאות עיגול, כמו אלה הנובעות מחישובים עם מספרים צפים, מדגימות שזוהי לא עניין טריוויאלי - הן יכולות לבלבל חישובים צבורית בתחומים כמו האווירו-חלל והפיננסים.

מעבר ל-`Math.round()`, יש לך את `BigDecimal`, המעניק לך שליטה דקדקנית יותר על הסקאלה והמודעות העיגול, ו-`DecimalFormat` עבור מקרים שבהם אתה צריך לעגל מספרים כחלק מעיצוב פלט טקסט. חלופות לעיגול כוללות הקטנה לערך השלם הקרוב ביותר מלמטה, העלאה לערך השלם הקרוב ביותר מלמעלה, וקיטום, שהם דרכים שונות לטפל בדיוק ובדרך כלל מוטפלות על ידי שיטות מחלקת `Math` שונות.

בהתאמה למקרה השימוש שלך, אסטרטגיית העיגול עשויה להשתנות. לדוגמה, `BigDecimal` נחשב לאידיאלי לחישובים פיננסיים, שם הדיוק קריטי. לעומת זאת, `Math.round()` הוא דרך מהירה לפעולות כלליות שבהן אתה פחות מסורבל לגבי המוד לעיגול.

## ראה גם
- [תיעוד Java Math של Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [תקן IEEE לחישוב צפים (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [מחלקת DecimalFormat ב-Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
