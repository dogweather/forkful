---
title:                "קבלת התאריך הנוכחי"
date:                  2024-03-08T21:55:28.884863-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-Dart כוללת שאילתת המערכת עבור התאריך והשעה הנוכחיים. הפונקציונליות הזו שימושית במיוחד באפליקציות לתכונות כמו הצבת חותמת זמן לאירועים, הצגת התאריך הנוכחי למשתמשים, או חישוב משכים. לדעת כיצד לאחזר ולנהל את התאריך הנוכחי בצורה יעילה היא בסיס לתזמון, רישום, ותכונות התלויות בזמן.

## איך לעשות זאת:
ספריית הליבה של Dart מספקת גישה ישירה לתאריך ולשעה הנוכחיים דרך מחלקת `DateTime`. להלן דוגמה בסיסית לקבלת התאריך הנוכחי:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // דוגמת פלט: 2023-04-12 10:00:00.000
}
```

אם אתם צריכים רק את חלק התאריך (שנה, חודש, יום), אתם יכולים לעצב את אובייקט `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // דוגמת פלט: 2023-04-12
}
```

Dart אינה כוללת ספרייה מובנית לעיצוב תאריכים מורכב יותר, אולם אתם יכולים להשתמש בחבילת `intl` למטרה זו. תחילה, הוסיפו את החבילה לקובץ `pubspec.yaml` שלכם:

```yaml
dependencies:
  intl: ^0.17.0
```

לאחר מכן, תוכלו לעצב תאריכים בקלות:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // דוגמת פלט: 2023-04-12
}
```

לאפשרויות עיצוב מתקדמות יותר, חקרו את מחלקת `DateFormat` שמספקת חבילת `intl`, התומכת במגוון רחב של דפוסים ואזורים.
