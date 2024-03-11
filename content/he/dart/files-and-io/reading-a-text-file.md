---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:03.753507-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\u05D9\u05E9\u05D4\
  \ \u05D5\u05D0\u05D7\u05D6\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\
  \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD\
  \ \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05E0\u05EA\u05D5\u05E0\
  \u05D9 \u05E7\u05DC\u05D8, \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\
  \u05E8\u05D4 \u05D0\u05D5 \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05E2\u05E8\u05DB\
  \u05D9\u2026"
lastmod: '2024-03-11T00:14:12.305178-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D5\
  \u05D0\u05D7\u05D6\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05D4\u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD\
  \ \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05E0\u05EA\u05D5\u05E0\
  \u05D9 \u05E7\u05DC\u05D8, \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\
  \u05E8\u05D4 \u05D0\u05D5 \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05E2\u05E8\u05DB\
  \u05D9\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט ב-Dart כוללת גישה ואחזור נתונים מקבצים המאוחסנים על מערכת הקבצים. תכנתים עושים זאת כדי לטפל בנתוני קלט, הגדרות תצורה או לקרוא מערכי נתונים, מה שהופך את זה לפעולה יסודית עבור הרבה יישומים שנעים מסקריפטים פשוטים ועד יישומים מורכבים.

## איך לעשות:

ספריית הליבה של Dart, `dart:io`, מספקת את הפונקציונליות הנדרשת לקריאת קבצי טקסט סינכרונית או אסינכרונית. הנה איך להתמודד עם שניהם.

**באופן סינכרוני:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // קריאת הקובץ באופן סינכרוני
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('שגיאה בקריאת הקובץ: $e');
  }
}
```

**באופן אסינכרוני:**

כדי למנוע חסימה של התוכנית בזמן שהקובץ נקרא, מועיל במיוחד עבור קבצים גדולים או יישומים תגובתיים:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('שגיאה בקריאת הקובץ: $e');
  }
}
```

**דוגמא לפלט:**

אם בקובץ הטקסט שלך נמצא:

```
Hello, Dart!
```

שתי השיטות לעיל יפיקו:

```
Hello, Dart!
```

**שימוש בספרייה צד שלישי:**

לתכונות נוספות כמו פעולות עבודה עם קבצים מופשטות או טיפול משופר בשגיאות, ייתכן שתיכנסו לשקול ספריות צד שלישי כמו `package:file`. עם זאת, לפי העדכון האחרון שלי, שימוש ישיר בחבילת הגרעין `dart:io`, כפי שהוצג לעיל, הוא השיטה הנפוצה והישירה ביותר לקריאת קבצי טקסט ב-Dart.
