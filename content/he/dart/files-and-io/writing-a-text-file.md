---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:38.046013-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\
  \u05D4 \u05D0\u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05E2\u05DC \u05D4\u05D3\u05D9\u05E1\u05E7 \u05DB\u05D3\u05D9 \u05DC\
  \u05D0\u05D7\u05E1\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 \u05D0\u05E4\u05DC\u05D9\
  \u05E7\u05E6\u05D9\u05D4, \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA, \u05DC\u05D5\u05D2\
  \u05D9\u05DD, \u05D0\u05D5\u2026"
lastmod: '2024-03-13T22:44:38.877921-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D0\u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\
  \u05DD \u05E2\u05DC \u05D4\u05D3\u05D9\u05E1\u05E7 \u05DB\u05D3\u05D9 \u05DC\u05D0\
  \u05D7\u05E1\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05E7\u05E8\u05D9\u05D0."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## מה ולמה?
כתיבת קובץ טקסט ב-Dart כוללת יצירה או שינוי של קבצים על הדיסק כדי לאחסן נתונים בפורמט קריא. מתכנתים עושים זאת כדי לשמור נתוני אפליקציה, הגדרות, לוגים, או כל מידע שצריך להישמר בין הפעלות של האפליקציה או לשתף נתונים עם אפליקציות או משתמשים אחרים.

## איך לעשות:
ספריית הליבה של Dart מספקת את חבילת ה-`dart:io` עבור טיפול בקבצים, ומאפשרת לך לכתוב קבצי טקסט בלי צורך בספריות מצד שלישי. הנה דוגמה פשוטה של כתיבת קובץ טקסט:

```dart
import 'dart:io';

void main() async {
  // יצירת קובץ חדש בשם 'example.txt' בתיקייה הנוכחית.
  var file = File('example.txt');
  
  // כתיבת מחרוזת לקובץ.
  await file.writeAsString('Hello, Dart!');
  
  // אימות התוכן.
  print(await file.readAsString()); // פלט: Hello, Dart!
}
```

כשמתמודדים עם קבצים גדולים יותר או זרמי נתונים, ייתכן שתעדיף לכתוב תוכן באמצעות `openWrite` שמחזיר `IOSink` ומאפשר לך לכתוב נתונים בחתיכות:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // כתיבת מספר שורות לקובץ.
  sink
    ..writeln('Line 1: The quick brown fox jumps over the lazy dog.')
    ..writeln('Line 2: Dart is awesome!')
    ..close();

  // המתנה לסגירת ה-sink כדי לוודא שכל הנתונים נכתבו לקובץ.
  await sink.done;

  // קריאה והדפסת תוכן הקובץ לאימות
  print(await file.readAsString());
}
```

לפעולות קובץ מתקדמות יותר, כולל הוספה לקבצים או כתיבת בייטים, ניתן לחקור עמוק יותר את שיטות המחלקה `File` המסופקת על ידי `dart:io`. בנוסף, כשעובדים על פרויקטים בקנה מידה גדול יותר או מורכבים יותר, שקול להשתמש בחבילות כמו `path` לטיפול בנתיבי קבצים או `shelf` לפונקציונליות של שרתי אינטרנט, על אף שכתיבת קבצים בדרך כלל מתבצעת על ידי ספריות המובנות של Dart.
