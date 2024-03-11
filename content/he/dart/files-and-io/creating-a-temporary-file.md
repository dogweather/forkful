---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:08.586952-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\
  \u05EA \u05E7\u05D5\u05D1\u05E5 \u05D4\u05DE\u05D9\u05D5\u05E2\u05D3 \u05DC\u05E9\
  \u05D9\u05DE\u05D5\u05E9 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05E7\u05E6\u05E8, \u05D1\
  \u05E2\u05D9\u05E7\u05E8 \u05DC\u05EA\u05E8\u05D7\u05D9\u05E9\u05D9\u05DD \u05DB\
  \u05DE\u05D5 \u05D4\u05D8\u05DE\u05E0\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05D0\u05D7\u05E1\u05D5\u05DF \u05D6\u05DE\u05E0\u05D9 \u05DC\u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05E7\u05D1\u05E6\u05D9\u05DD, \u05D0\u05D5 \u05E9\u05DE\u05D9\u05E8\
  \u05EA \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D4\u05D5\u05D0 \u05E8\u05D2\u05D9\u05E9\
  \ \u05DE\u05D3\u05D9\u2026"
lastmod: '2024-03-11T00:14:12.308736-06:00'
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E7\u05D5\u05D1\u05E5 \u05D4\u05DE\u05D9\u05D5\u05E2\u05D3 \u05DC\u05E9\u05D9\
  \u05DE\u05D5\u05E9 \u05DC\u05D8\u05D5\u05D5\u05D7 \u05E7\u05E6\u05E8, \u05D1\u05E2\
  \u05D9\u05E7\u05E8 \u05DC\u05EA\u05E8\u05D7\u05D9\u05E9\u05D9\u05DD \u05DB\u05DE\
  \u05D5 \u05D4\u05D8\u05DE\u05E0\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\
  \u05D7\u05E1\u05D5\u05DF \u05D6\u05DE\u05E0\u05D9 \u05DC\u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05E7\u05D1\u05E6\u05D9\u05DD, \u05D0\u05D5 \u05E9\u05DE\u05D9\u05E8\u05EA\
  \ \u05DE\u05D9\u05D3\u05E2 \u05E9\u05D4\u05D5\u05D0 \u05E8\u05D2\u05D9\u05E9 \u05DE\
  \u05D3\u05D9\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני ב-Dart כוללת יצירת קובץ המיועד לשימוש לטווח קצר, בעיקר לתרחישים כמו הטמנת נתונים, אחסון זמני לעיבוד קבצים, או שמירת מידע שהוא רגיש מדי לשמירה לאורך זמן. מתכנתים עושים זאת כדי לנהל נתונים שאינם זקוקים לאחסון קבוע, ובכך לשפר את הביצועים ולשמור על היגיינת נתונים.

## איך לעשות:
הספרייה `dart:io` של Dart מקלה על יצירת קבצים זמניים באמצעות הכיתה `Directory`. הנה דרך ישירה ליצור קובץ זמני ולכתוב תוכן אליו:

```dart
import 'dart:io';

Future<void> main() async {
  // יצירת תיקייה זמנית (במיקום ספציפי למערכת)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // יצירת קובץ זמני בתוך התיקייה הזו
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // כתיבת תוכן מסוים לקובץ הזמני
  await tempFile.writeAsString('This is some temporary content');

  print('קובץ זמני נוצר: ${tempFile.path}');

  // פלט לדוגמא: קובץ זמני נוצר: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### שימוש בספרייה של צד שלישי: `path_provider`

לאפליקציות (במיוחד אפליקציות לנייד עם Flutter), יכול להיות שתרצו ליצור קבצים זמניים בדרך יותר אחידה וניתנת לניהול. החבילה `path_provider` יכולה לעזור לכם למצוא את התיקייה הזמנית הנכונה בפלטפורמות שונות (iOS, Android וכו').

ראשית, הוסיפו את `path_provider` ל-`pubspec.yaml` שלכם תחת dependencies:

```yaml
dependencies:
  path_provider: ^2.0.9
```

והנה איך אפשר להשתמש בזה כדי ליצור קובץ זמני:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // קבלת התיקייה הזמנית
  final Directory tempDir = await getTemporaryDirectory();

  // יצירת קובץ זמני בתוך התיקייה הזו
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // כתיבת תוכן לקובץ הזמני
  await tempFile.writeAsString('This is some temporary content with path_provider');

  print('קובץ זמני נוצר עם path_provider: ${tempFile.path}');

  // פלט לדוגמא: קובץ זמני נוצר עם path_provider: /tmp/my_temp_file.txt (הנתיב יכול להשתנות בהתאם לפלטפורמה)
}
```

קטעי הקוד הללו ממחישים את יצירת והתעסקות עם קבצים זמניים ב-Dart, ומספקים גישה פשוטה ומעשית לניהול נתונים לצורכים לטווח קצר.
