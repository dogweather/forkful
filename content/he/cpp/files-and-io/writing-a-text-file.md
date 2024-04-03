---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.127831-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-C++ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\
  \u05EA \u05E7\u05D5\u05D1\u05E5 \u05D0\u05D5 \u05E4\u05EA\u05D9\u05D7\u05EA\u05D5\
  \ \u05D5\u05D0\u05D6 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05DC\u05EA\u05D5\u05DB\u05D5, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\
  \u05D5\u05D3\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8 \u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD \u05D4\u05D3\u05D5\u05E8\u05E9\u05D9\u05DD \u05E9\u05DE\u05D9\u05E8\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05DC\u05D5\u05D2\
  \u05D9\u05DD, \u05EA\u05D5\u05DB\u05DF \u05E9\u05E0\u05D5\u05E6\u05E8 \u05E2\u05DC\
  \ \u05D9\u05D3\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.866677-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-C++ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E7\u05D5\u05D1\u05E5 \u05D0\u05D5 \u05E4\u05EA\u05D9\u05D7\u05EA\u05D5 \u05D5\
  \u05D0\u05D6 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05DC\u05EA\u05D5\u05DB\u05D5, \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\u05D5\
  \u05D3\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8 \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\
  \u05DD \u05D4\u05D3\u05D5\u05E8\u05E9\u05D9\u05DD \u05E9\u05DE\u05D9\u05E8\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05DC\u05D5\u05D2\u05D9\
  \u05DD, \u05EA\u05D5\u05DB\u05DF \u05E9\u05E0\u05D5\u05E6\u05E8 \u05E2\u05DC \u05D9\
  \u05D3\u05D9 \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05D4\u05D2\u05D3\
  \u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\u05E8\u05D4."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## מה ולמה?
כתיבה לקובץ טקסט ב-C++ כוללת יצירת קובץ או פתיחתו ואז כתיבת נתונים לתוכו, משימה יסודית עבור יישומים הדורשים שמירת נתונים, כמו לוגים, תוכן שנוצר על ידי המשתמש או הגדרות תצורה. מתכנתים עושים זאת כדי לשמור נתונים שנוצרו במהלך ביצוע התוכנית או לייצא נתונים לשימוש על ידי תוכניות אחרות או משתמשים.

## איך ל:
C++ מציעה מספר דרכים לכתוב לקובץ טקסט, אך אחת השיטות הפשוטות ביותר היא שימוש בספרייה `<fstream>` שמספקת את המחלקה `ofstream` (זרם קובץ פלט) המיועדת לפעולות כתיבה לקובץ.

### דוגמה בשימוש ב-`<fstream>`:

```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "שלום, עולם!\n";
        file << "כתיבה לקובץ ב-C++ זה פשוט.";
        file.close();
    } else {
        std::cerr << "נכשל בפתיחת הקובץ\n";
    }
    return 0;
}
```

**פלט לדוגמה ב-'example.txt':**
```
שלום, עולם!
כתיבה לקובץ ב-C++ זה פשוט.
```

כשמתמודדים עם נתונים מורכבים יותר או זקוקים לשליטה רבה יותר בתהליך הכתיבה, מתכנתים עשויים להסתמך על ספריות צד שלישי כמו Boost Filesystem.

### דוגמה בשימוש ב-Boost Filesystem:

לשימוש ב-Boost לפעולות קובץ, תצטרכו להתקין תחילה את ספריות Boost. הדוגמה הבאה מדגימה יצירה וכתיבה לקובץ בעזרת `boost::filesystem` ו-`boost::iostreams`.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost הופך את פעולות הקובץ לקלות.\n";
    out << "זו שורה שנכתבה עם Boost.";
    
    return 0;
}
```

**פלט לדוגמה ב-'boost_example.txt':**
```
Boost הופך את פעולות הקובץ לקלות.
זו שורה שנכתבה עם Boost.
```

הבחירה בין C++ גולמי לבין ספרית צד שלישי כמו Boost עשויה להיות תלויה בדרישות הספציפיות של הפרויקט שלך ובכמה שליטה או גמישות אתה זקוק למבצעי I/O של הקובץ.
