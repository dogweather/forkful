---
title:                "חילוץ תת-מחרוזות"
date:                  2024-03-08T21:54:54.164217-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא פעולה שבה משיגים חלקים ספציפיים של מחרוזת על סמך המיקומים או התבניות שלהם. מתכנתים עושים זאת למשימות כמו ניתוח קלט מהמשתמש, מניפולציה של נתונים, או חילוץ מידע רלוונטי ממקורות טקסט גדולים יותר.

## איך לעשות זאת:
ב-Dart, ניתן להשתמש במספר שיטות לחילוץ תת-מחרוזות, כמו `substring()`, `split()`, וביטויים רגולריים. כל שיטה משרתת מטרות שונות ומציעה גמישות בטיפול במחרוזות.

### שימוש ב-`substring()`:
השיטה `substring()` היא ישירה. מציינים את האינדקס התחלתי (ואופציונלית, הסופי) כדי לחתוך את המחרוזת.

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // פלט: World
}
```

### שימוש ב-`split()`:
לפצל מחרוזת לרשימה של תת-מחרוזות על סמך תבנית (כמו רווח או פסיק), ואז לגשת לתת-המחרוזת לפי אינדקס.

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // גישה לפי אינדקס
  print(result); // פלט: is
}
```

### שימוש בביטויים רגולריים:
עבור תבניות מורכבות, הכיתה `RegExp` ב-Dart היא חזקה. ניתן להשתמש בה להתאמת תבניות ולחילוץ תת-מחרוזות.

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // פלט: example@mail.com
}
```

### ספריות צד שלישי:
למרות שספריית התקן של Dart יכולה להיות מספיקת יכולת, יכול להיות שתתקלו בתרחישים שבהם ספרייה של צד שלישי תפשט את משימתכם. בחירה פופולרית למניפולציה והתאמת תבניות של מחרוזות אינה מומלצת פה במיוחד כיוון שהיכולות הפנימיות של Dart לרוב מספיקות. עם זאת, תמיד בדקו ב-[pub.dev](https://pub.dev) עבור כל ספריות שעשויות להתאים יותר לצרכים הספציפיים שלכם.