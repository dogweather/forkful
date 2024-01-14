---
title:                "C: לעבוד עם קובץ csv"
simple_title:         "לעבוד עם קובץ csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-csv.md"
---

{{< edit_this_page >}}

# מדוע

CSV היא פורמט נתונים נפוץ לאחסון והצגת נתונים בקבצי טקסט פשוטים. פעמים רבות, מפתחי תוכניות נדרשים לעבוד עם קבצי CSV כדי לטעון, לעבוד ולשמור נתונים. בכתבה הזאת נלמד איך לתכנת בשפת C חיבור פשוט עם קבצי CSV ולעבוד עם הנתונים בתוכם.

# איך

התחברות לקובץ CSV קיים עושה שימוש בזוגת fopen ו fclose לפתיחה וסגירת קובץ. הפורמט בטקסט פשוט ניתן לטעון ידנית באמצעות fgets לקבלת שורה מהקובץ והשתמש בעזרת כלי לטעון נתונים נוספים. אנו נוכל להשתמש בזוגת sscanf כדי לעבור על כל שורת CSV ולשלוף את כל הנתונים באופן מסודר. לדוגמא:

```C
#include <stdio.h>

int main() {
    FILE *fptr;
    char buffer[255];
    fptr = fopen("data.csv", "r");
    while (fgets(buffer, 255, fptr)) {
        char name[255];
        int age;
        char occupation[255];
        sscanf(buffer, "%[^,],%d,%s", name, &age, occupation);
        printf("Name: %s\nAge: %d\nOccupation: %s\n\n", name, age, occupation);
    }
    fclose(fptr);
    return 0;
}
```

פעולת fopen מקבלת שני פרמטרים: הראשון הוא שם הקובץ, והשני הוא הידור הקובץ, כמו "r" עבור קריאה. fgets מקבלת שלושה פרמטרים: הראשון הוא מצביע למערך שאליו במקום לקחת את המידע, והשני הוא מספר תווים שיש לקרוא. הוא גם יקבל עליו כדי למצוא את הזמינו בתורה לתו ההבא. סטרטשר הוא לכתוב, בתור ASCII לעשות זאת אנו לכתוב לתוך. לא נתונה שורה באמצעות נקרא fgets, fgets מחזיר NULL הקריאים לסוף הקובץ.

# חקירה מעמיקה

ככל שנתקלת מחברת מקור חוץ מאפי