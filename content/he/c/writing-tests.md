---
title:                "כתיבת בדיקות"
html_title:           "C: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-tests.md"
---

{{< edit_this_page >}}

## למה

מפעם לפעם, אנו מתקלים בבעיות בקוד שלנו שמפריעות לנו להתקדם עם הפרויקטים שלנו. לכן, ישנו את הכוונה לכתוב טסטים כדי לבדוק את הקוד שלנו ולוודא שהוא עובד כפי שציפינו. כתיבת טסטים יכולה לחסוך לנו המון זמן ומאמץ בעתיד, כאשר אנו נעשה שינויים בקוד שלנו.

## איך לעשות זאת

כתיבת טסטים ב-C היא פשוטה ונמצאת בטווח של כל מפתח. הנה כמה דוגמאות קוד ותוצאות להראות לכם כיצד לכתוב טסטים פשוטים ב-C.

```C
// הגדרת מילון ופונקציית תוצאה עבור פיצה עם נתוני טיפול מורחבים
struct Pizza {
    char *name;
    int size;
    int slices;
};

int pizza_total_slices(struct Pizza pizza) {
    return pizza.size * pizza.slices;
}

// בדיקה שהפונקציה שיקבל את הפיצה הנכונה
void test_pizza_total_slices() {
    struct Pizza pizza = {"Pepperoni", 12, 8};
    assert(pizza_total_slices(pizza) == 96);
}

int main() {
    test_pizza_total_slices();
}
```
תוצאה:
```
Successfully tested pizza_total_slices function.
```

כאן אנו משתמשים בהגדרת מילון כדי ליצור נתוני פיצה משתנם ובמבנה כדי לבדוק את התוצאה. בדיקות כאלה יכולות להיות מועילות במיוחד כאשר אנו עובדים עם מחרוזות או טפלים מסובכים יותר.

## עוד פרטים

ישנם כמה טכניקות שונות לכתיבת טסטים ב-C, כולל שימוש בספריות כמו CUnit ו-GTest, או השתמשות בדוגמאות קוד מהגוף הראשון. מה שחשוב לזכור הוא שכיתוב טסטים משמש לא רק לבדיקת נתונים מצופים, אלא גם לניסיונות וכיפוף של הקוד שלנו. על התוצאות מהטסטים להיות