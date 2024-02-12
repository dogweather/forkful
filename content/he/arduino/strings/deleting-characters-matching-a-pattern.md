---
title:                "מחיקת תווים התואמים לתבנית"
aliases: - /he/arduino/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:54.038591-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת תווים התואמים לתבנית היא פרקטיקה שבה אנו מחפשים תווים מסוימים במחרוזת ומוחקים אותם. תכניתנים עושים זאת כדי לטהר קלט, הסרת פורמטינג או לאמת נתונים.

## איך לעשות:
ארדואינו לא תומך באופן מובנה בביטויים רגולריים. נצטרך לעשות זאת ידנית. הנה דוגמא:

```Arduino
String removePattern(String str, char toRemove) {
  String result = "";
  for (unsigned int i = 0; i < str.length(); i++) {
    if (str[i] != toRemove) result += str[i];
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String data = "Hello-World-123";
  Serial.println(removePattern(data, '-'));  // "HelloWorld123"
}

void loop() {
  // Nothing to do here
}
```

פלט הדוגמה: "HelloWorld123"

## עיון מעמיק:
עד לשנות ה-70, כאשר עיבוד טקסט התחיל להיות נפוץ יותר, לא הייתה דרך פשוטה להסיר תווים לפי תבנית. ביטויים רגולריים, שנוצרו בשנת 1956 על ידי סטיבן קליני, שיפרו את היכולת לחפש ולעבד טקסט באופן יעיל. קוד הארדואינו נועד להיות פשוט ואינו כולל תמיכה רשמית בביטויים רגולריים, מכיוון שלוקח הרבה משאבים במערכת עם משאבים חסוכים. דרך חלופית היא להשתמש בספריות צד שלישי שנועדו לארדואינו, אך יש לשים לב לשימוש בזיכרון ובמשאבי מעבד.

## ראה גם:
- [מדריך ארדואינו רשמי](https://www.arduino.cc/reference/en/)
- [דוקומנטציה למחרוזות בארדואינו](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [פורום התמיכה של ארדואינו](https://forum.arduino.cc/)
- [ביטויים רגולריים](https://en.wikipedia.org/wiki/Regular_expression) (להבנה כללית, לא ספציפית לארדואינו)
