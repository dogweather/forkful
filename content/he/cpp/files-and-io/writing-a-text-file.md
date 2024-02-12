---
title:                "כתיבת קובץ טקסט"
aliases:
- /he/cpp/writing-a-text-file.md
date:                  2024-02-03T19:28:10.127831-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת קובץ טקסט"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
