---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:08.016884-07:00
description: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA (REPL - Read-Evaluate-Print Loop) \u05E2\u05D1\u05D5\
  \u05E8 Dart \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DC\u05D4\u05E7\u05DC\u05D9\u05D3 \u05D5\u05DC\u05D1\u05E6\u05E2\
  \ \u05E7\u05D5\u05D3 Dart \u05D1\u05D0\u05D5\u05E4\u05DF \u05D3\u05D9\u05E0\u05DE\
  \u05D9 \u05E9\u05D5\u05E8\u05D4 \u05D0\u05D7\u05E8 \u05E9\u05D5\u05E8\u05D4 \u05D1\
  \u05DC\u05D9 \u05E6\u05D5\u05E8\u05DA \u05DC\u05D4\u05D3\u05E8 \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.848601-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA (REPL - Read-Evaluate-Print Loop) \u05E2\u05D1\u05D5\
  \u05E8 Dart \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DC\u05D4\u05E7\u05DC\u05D9\u05D3 \u05D5\u05DC\u05D1\u05E6\u05E2\
  \ \u05E7\u05D5\u05D3 Dart \u05D1\u05D0\u05D5\u05E4\u05DF \u05D3\u05D9\u05E0\u05DE\
  \u05D9 \u05E9\u05D5\u05E8\u05D4 \u05D0\u05D7\u05E8 \u05E9\u05D5\u05E8\u05D4 \u05D1\
  \u05DC\u05D9 \u05E6\u05D5\u05E8\u05DA \u05DC\u05D4\u05D3\u05E8 \u05E1\u05E7\u05E8\
  \u05D9\u05E4\u05D8\u05D9\u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E7\u05DC\u05D9\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?

מעטפת אינטראקטיבית (REPL - Read-Evaluate-Print Loop) עבור Dart מאפשרת למתכנתים להקליד ולבצע קוד Dart באופן דינמי שורה אחר שורה בלי צורך להדר סקריפטים שלמים. כלי זה חיוני ללמידת תחביר Dart, לניסוי עם קטעי קוד, או לניפוי באגים, על ידי הצעת משוב מיידי וקידום בדיקות איטרטיביות.

## איך לעשות:

Dart לא מגיע עם יכולת REPL מובנית. עם זאת, ניתן להשיג פונקציונליות דומה ל-REPL באמצעות DartPad (באינטרנט) או על ידי השימוש בכלים של צד שלישי כמו `dart_repl`.

**שימוש ב-DartPad:**

DartPad (https://dartpad.dev) הוא עורך Dart מקוון שמאפשר לכתוב ולהריץ קוד Dart בדפדפן האינטרנט שלך. אף על פי שהוא לא REPL קלאסי משורת הפקודה, הוא מספק חוויה דומה לניסוי מהיר.

פשוט עבור לאתר, הקלד את קוד ה-Dart שלך בחלונית השמאלית, ולחץ על "הפעל" כדי לראות את הפלט בצד הימני.

דוגמה:
```dart
void main() {
  print('שלום, Dart!');
}
```
פלט:
```
שלום, Dart!
```

**שימוש ב-`dart_repl` (כלי של צד שלישי):**

ראשית, התקן את `dart_repl` באמצעות pub באופן גלובלי:

```shell
dart pub global activate dart_repl
```

לאחר מכן, הרץ את `dart_repl` מהטרמינל שלך:

```shell
dart_repl
```

עכשיו, תוכל להתחיל להקליד פקודות Dart ישירות במעטפת. לדוגמה:

```dart
>>> print('שלום, REPL!');
שלום, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

שיטות אלו מספקות דרך מהירה לניסוי עם קוד Dart על המקום, מה שמקל באופן משמעותי על עקומת הלמידה ומשפר את הפרודוקטיביות.
