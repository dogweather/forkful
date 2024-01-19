---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה? 
צירוף מחרוזות הוא הפעולה שבה מתחברות שתי מחרוזות או יותר למחרוזת אחת ארוכה יותר. מתכנתים משתמשים בציקוף מחרוזות כדי ליישר הודעות, תמיכה בהכנסת נתונים דינמיים למחרוזות, ועוד.

## איך לעשות:
נסו את משלבי הקוד הבא ב-Arduino שלכם כדי לראות איך מסתדר צירוף של מחרוזות.

בקוד ה-Arduino הנ"ל, ישנן שתי מחרוזות - startText ו-endText, שהן מתצירפות למחרוזת משותפת. 

```Arduino
String startText = "ברוכים הבאים ";
String endText = "למערכת שלנו!";
String bothTexts = startText + endText; // המחרוזת רובעת את הצירוף
Serial.println(bothTexts); // ברוכים הבאים למערכת שלנו!
```

## בדיוק היסטורי:
מתחילה, בשפת ה-C, הצירוף הושג על ידי שימוש בפונקציית strcpy() ו-strcat(). אבל, Arduino, המבוססת על C++, מציעה דרך קלה ונוחה יותר לעשות זאת, נקראת operator cats (+). יש יתרונות וחסרונות לכל שיטה: השיטה הישנה יותר מהירה אך מסורבלת, בעוד שהשיטה החדשה יותר איטית אך משתמשת בלוגיקה פשוטה יותר.

## ראה גם:
- [מדריך מקיף לעבודה עם מחרוזות ב-Arduino](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)
- [מסמך מרכזי בנושא מחרוזות](http://www.cplusplus.com/reference/string/string/)
- [הסבר איך לשלב מחרוזות בשפת C](https://www.cplusplus.com/reference/cstring/strcat/)