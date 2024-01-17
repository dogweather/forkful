---
title:                "עבודה עם csv"
html_title:           "C: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ועל מה?
העבודה עם קבצי CSV היא חלק חשוב מהתפקיד של מתכנתים. CSV הוא פורמט נתונים המשמש לקריאה וכתיבה של נתונים מסודרים בפשטות, מה שהופך אותו לנפוץ ושימושי בתחום התכנות.

## כיצד לעשות זאת:
נהליך לפתרון הכי נפוץ ופשוט ביותר חלוף. הנה דוגמאות ותוצאות ממשקי קוד :C

```C
שם קובץ CSV: התאמה לקריאה ולכתיבה

תוכן הנתונים:

id, name, age 
1, John, 30 
2, Sarah, 25 
3, Michael, 35

קוד הקריאה:
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main()
{
    FILE* file = fopen("example.csv", "r"); // פתיחת קובץ לקריאה
    if (file == NULL)
    {
        perror("לא ניתן לפתוח את הקובץ");
        return -1;
    }

    char line[100];
    char* token; // משתנה להצבעת הנתונים
    const char* delimiter = ","; // המתו המבוקש לחלק את הנתונים ביניהם

    printf("התוכן של הקובץ:\n");
    while (fgets(line, 100, file) != NULL) // קריאת כל שורות הקובץ עד שהיא מודפסת ראשונה
    {
        token = strtok(line, delimiter);
        while (token != NULL)
        {
            printf("%s ", token); // repeat prints the token
            token = strtok(NULL, delimiter);
        }

        printf("\n");
    }

    fclose(file); // סגירת הקובץ לסיום פתיחתו 

    return 0;
}

קוד פלט לקובץ CSV:
התוכן של הקובץ:
id  name  age
1  John  30
2  Sarah  25
3  Michael  35
```

## חקירה מעמיקה:
פורמט CSV פותח בשנות ה -70 כדי לציין את הקשרים האוטומטי שבין שדות הטקסט בבסיסי נתונים. כיום, ישנם מספר פתרונות אחרים לנתונים מבנק הנתונים כהתחלה. רעיונות כאלה כוללים JSON, XML וכברירת מחדל כי כך הוא פועל

הפוגשים בצורך לשמור נתונים בדיסק כדי לשלוט בהם לעבוד כרגע.

## ראו גם:
נוליןקים לדברים דומים והשוואה עם כוונת המידע הזה. כל קשר שהם מייצגים לחברת (בפורמט CSV? זה כל סימן שאתה יכול לדמות.)