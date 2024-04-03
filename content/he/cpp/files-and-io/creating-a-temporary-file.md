---
date: 2024-01-20 17:40:16.437770-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1\u05E7\u05D5\u05D3 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05E9\
  \u05DE\u05D9\u05E8\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05D6\u05DE\u05E0\u05D9 \u05DC\u05DC\u05D0 \u05D4\u05E9\
  \u05E4\u05E2\u05D4 \u05E2\u05DC \u05D4\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D9\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E6\u05D5\u05E8\u05DA \u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA, \u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\u05DD \u05D1\
  \u05D9\u05E0\u05D9\u05D9\u05DD, \u05D5\u05D0\u05D7\u05E1\u05E0\u05EA \u05DE\u05D9\
  \u05D3\u05E2 \u05D1\u05D8\u05D5\u05D7\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.868353-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1\u05E7\u05D5\u05D3 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05E9\u05DE\
  \u05D9\u05E8\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D6\u05DE\u05E0\u05D9 \u05DC\u05DC\u05D0 \u05D4\u05E9\u05E4\
  \u05E2\u05D4 \u05E2\u05DC \u05D4\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D9\
  \u05D9\u05DE\u05EA."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

## מה ולמה?

יצירת קובץ זמני בקוד מאפשרת שמירה של נתונים באופן זמני ללא השפעה על המערכת הקיימת. תכניתנים עושים זאת לצורך בדיקות, חישובים ביניים, ואחסנת מידע בטוחה שתימחק לאחר שימוש.

## איך לעשות:

הנה דוגמא קצרצרה ליצירת קובץ זמני ב-C++ עם שימוש בספרייה `<filesystem>`:

```C++
#include <iostream>
#include <filesystem>
#include <fstream>

int main() {
    std::filesystem::path temp = std::filesystem::temp_directory_path() / "my_temp_file.txt";
    std::ofstream temp_file(temp);
    temp_file << "This is some temporary data.";
    temp_file.close();
    
    // Use the file...

    // When done, delete the temporary file
    std::filesystem::remove(temp);
    
    return 0;
}
```

תוצאת דוגמא: המשך זה יצור קובץ זמני במערכת, יכתוב נתונים אליו, ואז ימחק אותו.

## צלילה לעומק:

בעבר, יצירת קובץ זמני עלולה הייתה להיות עניין פחות ישיר ומשולב עם סיכוני אבטחת מידע, כי על התכניתן עצמו נדרש לדאוג לניקוי הקבצים. כיום, ספריות רבות מעניקות פתרונות פחות פגיעים. ישנם גם חלופות כגון שימוש ב-memory-mapped files או במאגרי נתונים לעיבוד אסינכרוני של נתונים. בדרך כלל, קבצים זמניים ייצרו בספריית הפעלה (operating system) או בתיקיית משתמש ספציפית, כאשר ניתן לציין זאת באופן מדויק באמצעות ה-C++ וה-API שלו.

## גם זה כדאי:

- [cppreference/std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [C++17 Filesystem Tutorial](https://www.youtube.com/watch?v=8kZBszfTzi4)
- [ISO C++ - Filesystem](https://isocpp.org/wiki/faq/cpp17-library#filesystem)

חוץ ממדריכים והסברים, הסתכלות על התיעוד של הסטנדרט עצמו יכולה לעזור להבין את התשתית ואת האפשרויות המלאות הזמינות.
