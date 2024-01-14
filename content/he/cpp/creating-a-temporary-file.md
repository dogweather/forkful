---
title:    "C++: יצירת קובץ זמני"
keywords: ["C++"]
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא תהליך חשוב בתכנות בשפת C++. קבצים זמניים משמשים כדי לאחסן מידע זמני בתוך תוכנית ובסופן נמחקים לגמרי. זה יכול לסייע ליצירת תוכניות יעילות יותר ולחסוך במשאבים.

## כיצד ליצור קובץ זמני בשפת C++

על מנת לצור קובץ זמני בשפת C++, ניתן להשתמש בפונקציית `tmpfile()` שמוגדרת בספריית התקנה הסטנדרטית `cstdio`. פונקציה זו מחזירה מצביע לקובץ זמני שנוצר בזמן הריצה של התוכנית.

```C++
#include <cstdio>
#include <iostream>

int main() {
    FILE* temp_file = tmpfile();
    if (temp_file == NULL) {
        std::cout << "Failed to create temporary file!" << std::endl;
    }
    else {
        std::cout << "Successfully created temporary file." << std::endl;
        // Use the temp file for temporary data storage
    }
}
```
הפלט של הקוד המופעל יראה כך:

```
Successfully created temporary file.
```

## Deep Dive

כעת שהבנו איך ליצור קובץ זמני בשפת C++, ניתן לעשות קפיצה קדימה וללמוד עוד על פונקציית `tmpfile()`. פונקציה זו מקבלת כפרמטרים נוספים את התיקייה שבה הקובץ יוצר ואת הפורמט שלו.

בנוסף, כדי להבטיח שהקובץ יימחק בסיום התוכנית, יש להשתמש בפונקציית `fclose()` עם המצביע לקובץ זמני כדי לסגור אותו ולהסיר אותו מהמערכת.

עוד דבר חשוב לזכור הוא שקבצים זמניים נמחקים אוטומטית בסיום התוכנית, אבל יש לזכור לסגור אותם כדי למנוע מנצחים במערכת ההפעלה.

## See Also

- [פונקציית tmpfile() במדריך לשפת C++](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/tmpfile-tmpnam-mktemp-wtmpnam-s-wtmpnam-s-functions?view=msvc-160)
- [קבצים