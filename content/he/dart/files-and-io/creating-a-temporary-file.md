---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:08.586952-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 `dart:io` \u05E9\u05DC Dart \u05DE\u05E7\u05DC\u05D4 \u05E2\
  \u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD \u05D6\u05DE\
  \u05E0\u05D9\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DB\
  \u05D9\u05EA\u05D4 `Directory`. \u05D4\u05E0\u05D4 \u05D3\u05E8\u05DA \u05D9\u05E9\
  \u05D9\u05E8\u05D4 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05D6\
  \u05DE\u05E0\u05D9 \u05D5\u05DC\u05DB\u05EA\u05D5\u05D1 \u05EA\u05D5\u05DB\u05DF\
  \ \u05D0\u05DC\u05D9\u05D5."
lastmod: '2024-03-13T22:44:38.879641-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 `dart:io` \u05E9\u05DC Dart \u05DE\
  \u05E7\u05DC\u05D4 \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05D6\u05DE\u05E0\u05D9\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA \u05D4\u05DB\u05D9\u05EA\u05D4 `Directory`."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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
