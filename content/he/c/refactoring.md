---
title:                "רפקטורינג"
date:                  2024-01-26T01:17:33.871713-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא התהליך של שינוי מבנה קוד מחשב קיים ללא שינוי התנהגותו החיצונית. מתכנתים עושים זאת לשיפור הקריאות, הפחתת המורכבות, או להפוך את הקוד ליותר ניתן לתחזוקה וסקלבילי, מה שיכול לחסוך המון זמן וכאבי ראש לאורך זמן.

## איך לעשות:
בואו נשדרג קצת קוד. דמיינו שיש לכם פונקציה שמחשבת את הממוצע של מספרים שלמים במערך. במבט ראשון, זה קצת מבולגן.

**לפני ריפקטורינג:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // הוספת לסכום בתנאי הלולאה for, אוי!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateStuff(array, length));

    return 0;
}
```

**אחרי ריפקטורינג:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateAverage(array, length));
    return 0;
}
```
אפילו עם דוגמה פשוטה זו, אתם יכולים לראות איך פיצול הפונקציה הופך את הקוד לנקי יותר וניתן יותר לתחזוקה. כעת לכל פונקציה יש אחריות בודדת – עקרון מפתח בקידום קוד נקי.

## צלילה עמוקה
המונח "ריפקטורינג" הופך לפופולרי בסוף שנות ה-90, במיוחד עם פרסום הספר של מרטין פאולר "Refactoring: Improving the Design of Existing Code." ריפקטורינג לא משמעותו תיקון באגים או הוספת מאפיינים חדשים, אלא שיפור מבנה הקוד.

ישנם כלים וסביבות פיתוח משולבות (IDEs) רבים לריפקטורינג שעוזרים לאוטמט את התהליך, כמו CLion עבור C ו-C++, אך להבין מה קורה מאחורי הקלעים נותר חשוב.

חלופות לריפקטורינג יכולות לכלול כתיבה מחדש של קוד (מסוכן ולעיתים קרובות לא נחוץ) או התמודדות עם החוב הטכנולוגי (שיכול להיות יותר יקר לטווח הארוך). פרטי המימוש משתנים בהתאם לפרויקט, אך שיפורים נפוצים כוללים שינוי שמות של משתנים לשם יותר ברור, פיצול פונקציות גדולות לקטנות יותר, והחלפת מספרים קסומים בקבועים עם שמות.

כמו כן, דפוסים כמו DRY (Don't Repeat Yourself) ועקרונות SOLID יכולים להנחות את מסע הריפקטורינג שלכם, מקדמים בסיס קוד שהוא קל יותר לבדוק, להבין ולשתף פעולה עליו.

## ראו גם
לצלילה עמוקה יותר בעולם הריפקטורינג, תסתכלו על:

- דף הבית של מרטין פאולר: https://martinfowler.com/ עם אוצר של מאמרים ומשאבים על ריפקטורינג ועיצוב תוכנה.
- Refactoring.com: https://refactoring.com/ מספק דוגמאות וקטלוגים של טכניקות ריפקטורינג.
- הספר "Refactoring": נחשב לתנ"ך של ריפקטורינג, קריאתו נותנת תצפית מלאה על המתודולוגיה.
- "Clean Code: A Handbook of Agile Software Craftsmanship" מאת רוברט סי. מרטין, שדן בכתיבת קוד שקל להבין ולתחזק.
