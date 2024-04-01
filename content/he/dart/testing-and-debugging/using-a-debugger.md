---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:03.639284-07:00
description: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05EA\u05D7 \u05EA\
  \u05E7\u05DC\u05D5\u05EA \u05D1-Dart \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D1\u05D7\u05D5\u05DF \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05DE\u05E1\u05D5\u05D3\u05E8 \u05D0\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05E9\u05DC\u05D4\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05D2\u05D3\
  \u05E8\u05EA \u05E0\u05E7\u05D5\u05D3\u05D5\u05EA \u05E2\u05E6\u05D9\u05E8\u05D4\
  , \u05D4\u05EA\u05E7\u05D3\u05DE\u05D5\u05EA \u05D3\u05E8\u05DA \u05D1\u05D9\u05E6\
  \u05D5\u05E2 \u05D4\u05E7\u05D5\u05D3, \u05D5\u05D1\u05D3\u05D9\u05E7\u05EA \u05DE\
  \u05E9\u05EA\u05E0\u05D9\u05DD. \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05D4\
  \u05DB\u05E8\u05D7\u05D9\u2026"
lastmod: '2024-03-13T22:44:38.853774-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05EA\u05D7 \u05EA\u05E7\
  \u05DC\u05D5\u05EA \u05D1-Dart \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05D1\u05D7\u05D5\u05DF \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05DE\u05E1\u05D5\u05D3\u05E8 \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05D4\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05D2\u05D3\u05E8\
  \u05EA \u05E0\u05E7\u05D5\u05D3\u05D5\u05EA \u05E2\u05E6\u05D9\u05E8\u05D4, \u05D4\
  \u05EA\u05E7\u05D3\u05DE\u05D5\u05EA \u05D3\u05E8\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05E7\u05D5\u05D3, \u05D5\u05D1\u05D3\u05D9\u05E7\u05EA \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD. \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05D4\u05DB\
  \u05E8\u05D7\u05D9\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05EA\u05D7 \u05EA\u05E7\
  \u05DC\u05D5\u05EA"
---

## איך לעשות:


### ניפוי באגים בסיסי:
**1. הגדרת נקודות עצירה:**

כדי להגדיר נקודת עצירה, פשוט לחצו על שוליים השמאליים של שורת הקוד בסביבת ה-IDE שלכם (לדוגמא, Visual Studio Code או Android Studio) במקום שבו אתם רוצים שהביצוע יעצור.

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // הגדר כאן נקודת עצירה
}
```

**2. התחלת ניפוי באגים:**

ב-IDE שלכם, התחילו סשן ניפוי באגים על ידי לחיצה על סמל הניפוי או לחיצה על כפתור הניפוי. הביצוע יעצור בנקודות העצירה.

**3. בדיקת משתנים:**

כאשר הביצוע עוצר, העבירו את העכבר מעל משתנים כדי לראות את ערכיהם הנוכחיים.

**4. התקדמות דרך הקוד:**

השתמשו בפקודות צעד קדימה, צעד פנימה, וצעד החוצה ב-IDE שלכם כדי לנווט דרך הקוד שלכם שורה או פונקציה בכל פעם.

### ניפוי באגים מתקדם עם Observatory:
Dart כוללת כלי בשם Observatory עבור ניפוי באגים ופרופילינג של אפליקציות Dart. זה במיוחד שימושי עבור אפליקציות הרצות על VM של Dart.

**גישה ל-Observatory:**

הריצו את האפליקציה שלכם ב-Dart עם הדגל `--observe`.

```bash
dart --observe your_program.dart
```

פקודה זו מדפיסה URL לקונסול, שאותו ניתן לפתוח בדפדפן כדי לגשת למנתח התקלות של Observatory.

### שימוש בספריות צד שלישי פופולריות:
עבור ניפוי באגים של אפליקציות Flutter, החבילה `flutter_devtools` מספקת ערכת כלים לביצועים וניפוי באגים המשתלבת עם VM של Dart ו-Flutter.

**התקנה:**

ראשית, הוסיפו את `devtools` לקובץ `pubspec.yaml` שלכם תחת `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**הפעלת DevTools:**

הפעילו פקודה זו בטרמינל שלכם:

```bash
flutter pub global run devtools
```

לאחר מכן, התחילו את האפליקציה שלכם ב-Fultter במצב ניפוי באגים. DevTools מספקת תכונות כמו פקח Flutter לניתוח עץ הווידג'טים, והפרופיילר הרשתי לניטור פעילות רשת.

### פלט לדוגמא:
בעת הגעה לנקודת עצירה, ה-IDE שלכם עשוי להציג ערכי משתנים ועקבות מחסנית כך:

```
message: 'Hello, Debugging'
```

על ידי שימוש יעיל בכלים ובטכניקות ניפוי באגים ב-Dart, מפתחים יכולים לזהות ולפתור בעיות בצורה מהירה יותר, מה שמוביל לתהליך פיתוח חלק יותר ולאפליקציות חזקות יותר.
