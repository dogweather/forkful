---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:43.668181-07:00
description: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1-Dart \u05DE\u05E2\u05E8\u05D1\u05EA \u05D4\u05DB\u05E0\
  \u05EA \u05E1\u05D1\u05D9\u05D1\u05D4 \u05DE\u05D5\u05E2\u05D9\u05DC\u05D4 \u05DC\
  \u05E4\u05D9\u05EA\u05D5\u05D7, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D5\u05D4\u05E4\
  \u05E6\u05D4 \u05D9\u05E2\u05D9\u05DC\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D9\u05D9\u05E1\u05D3\u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\
  \u05E7\u05D8\u05D9\u05DD \u05D7\u05D3\u05E9\u05D9\u05DD \u05D1-Dart \u05DB\u05D3\
  \u05D9 \u05DC\u05E0\u05E6\u05DC \u05D0\u05EA \u05D4\u05D1\u05D9\u05E6\u05D5\u05E2\
  \u05D9\u05DD \u05D4\u05D0\u05D5\u05E4\u05D8\u05D9\u05DE\u05DC\u05D9\u05D9\u05DD\
  \ \u05D5\u05D0\u05EA\u2026"
lastmod: '2024-03-09T21:06:03.628023-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1-Dart \u05DE\u05E2\u05E8\u05D1\u05EA \u05D4\u05DB\u05E0\u05EA\
  \ \u05E1\u05D1\u05D9\u05D1\u05D4 \u05DE\u05D5\u05E2\u05D9\u05DC\u05D4 \u05DC\u05E4\
  \u05D9\u05EA\u05D5\u05D7, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D5\u05D4\u05E4\u05E6\
  \u05D4 \u05D9\u05E2\u05D9\u05DC\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05D9\u05D9\u05E1\u05D3\u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\u05E7\
  \u05D8\u05D9\u05DD \u05D7\u05D3\u05E9\u05D9\u05DD \u05D1-Dart \u05DB\u05D3\u05D9\
  \ \u05DC\u05E0\u05E6\u05DC \u05D0\u05EA \u05D4\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\
  \u05DD \u05D4\u05D0\u05D5\u05E4\u05D8\u05D9\u05DE\u05DC\u05D9\u05D9\u05DD \u05D5\
  \u05D0\u05EA\u2026"
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?

התחלת פרויקט חדש ב-Dart מערבת הכנת סביבה מועילה לפיתוח, בדיקה והפצה יעילים. מתכנתים מייסדים פרויקטים חדשים ב-Dart כדי לנצל את הביצועים האופטימליים ואת האקוסיסטם הרחב של Dart, במיוחד לפיתוח יישומי ווב וניידים עם מסגרות כמו Flutter.

## איך לעשות:

1. **התקנת Dart**:
   וודא ש-Dart מותקן במערכת שלך. אם לא, תוכל להוריד אותו מ-[https://dart.dev/get-dart](https://dart.dev/get-dart). אמת את ההתקנה באמצעות:

   ```shell
   dart --version
   ```

2. **יצירת פרויקט Dart חדש**:
   השתמש ב-Dart CLI כדי ליצור פרויקט חדש:

   ```shell
   dart create hello_dart
   ```

   פקודה זו יוצרת ספרייה חדשה `hello_dart` עם יישום דוגמה פשוט לאינטרנט או לקונסול, תלוי בבחירה שלך.

3. **בדוק את מבנה הפרויקט**:
   
   נווט לספריית הפרויקט שלך:

   ```shell
   cd hello_dart
   ```

   פרויקט Dart טיפוסי כולל את הקבצים והספריות החשובים הבאים:

   - `pubspec.yaml`: קובץ תצורה שכולל את תלותות הפרויקט ואילוצי ה-SDK.
   - `lib/`: הספרייה בה רוב הקוד של Dart שוכן.
   - `test/`: ספרייה לבדיקות הפרויקט.

4. **הוסף תלותות**:
   ערוך את `pubspec.yaml` כדי להוסיף תלותות. לפרויקטים לאינטרנט, שקול להוסיף את `http`, חבילה פופולרית לביצוע בקשות HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   לאחר העריכה, קבל את התלותות:

   ```shell
   dart pub get
   ```

5. **כתוב את קוד ה-Dart הראשון שלך**:
   
   בספריית `lib/`, צור קובץ Dart חדש, `main.dart`, והוסף קוד Dart פשוט:

   ```dart
   // ייבוא של הספרייה המרכזית של Dart
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **הרץ את יישום ה-Dart שלך**:

   בצע את התוכנית שלך ב-Dart על ידי:

   ```shell
   dart run
   ```

   הפלט צריך להיות:

   ```
   Hello, Dart!
   ```

על ידי ביצוע השלבים האלו, הצלחת להתחיל פרויקט Dart חדש, מההתקנה ועד להרצת קטע הקוד הראשון שלך ב-Dart. ידע זה מהווה בסיס להעמקה נוספת באקוסיסטם העשיר של Dart וביכולותיו לבניית יישומים בר קנה מידה.
