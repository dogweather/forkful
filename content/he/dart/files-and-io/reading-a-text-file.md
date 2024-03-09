---
title:                "קריאת קובץ טקסט"
date:                  2024-03-08T21:56:03.753507-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
