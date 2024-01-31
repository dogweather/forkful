---
title:                "עיגול מספרים"
date:                  2024-01-26T03:44:51.787039-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים הוא קיצוץ של הספרות מעבר לנקודה מסוימת תוך שמירה אופציונלית על הספרה האחרונה שנשמרת. תכנתים מעגלים כדי להפחית דיוק כאשר ערכים מדויקים אינם נחוצים, לנהל שגיאות נקודה צפה או להכין מספרים לתצוגה ידידותית למשתמש.

## איך לעשות:
בשפת C, בדרך כלל תשתמשו בפונקציות `floor()`, `ceil()`, או `round()`. הנה הדגמה מהירה:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("רצפה: %.2f\n", num_floor); // רצפה: 3.00
    printf("תקרה: %.2f\n", num_ceil); // תקרה: 4.00
    printf("עיגול: %.2f\n", num_round); // עיגול: 3.00
    return 0;
}
```

לשליטה רבה יותר, כמו לעיגול למקום מסוים, תכפילו, תעגלו ותחלקו:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("עוגל ל-2 ספרות אחרי הנקודה: %.2f\n", num_rounded); // עוגל ל-2 ספרות אחרי הנקודה: 3.14
```

## צלילה עמוקה
בעבר, עיגול לעתים קרובות אומר תהליך ידני – משאב כבד עם עט ונייר בלבד. עם המחשוב, אוטמטנו תהליך זה, אבל החישוב בנקודות צפות הביא לנו עדינויות בשל הטבע הבינארי שלה, שבו כמה מספרים לא יכולים להיות מיוצגים במדויק.

חלופות לעיגול סטנדרטי כוללות קיצוץ (פשוט השמטת ספרות נוספות) או עיגול של בנקאים, אשר מעגל למספר הזוגי הקרוב ביותר כאשר הוא ממש בין שני ערכים, מה שמפחית הטיה בחישובים חוזרים.

היישום מסתבך כאשר צריך לעגל מספרים בדיוק שרירותי או לטפל במקרים מיוחדים כמו אינסוף, NaNs מסמנים, או ערכים משניים. פונקציות הספרייה הסטנדרטית של C מטפלות ביסודות, אך אם צריך לעגל עשרוניים בדרכים מותאמות אישית, יהיה צורך ביותר מ-`math.h`.

## ראו גם
- [תיעוד `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [חישוב נקודה צפה](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [המלכודות באימות חישובים בנקודה צפה](https://dl.acm.org/doi/10.1145/1186736.1186737)
