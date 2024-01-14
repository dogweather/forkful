---
title:                "Arduino: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# למה

ניצול של סיבובי תווים נחשבת לכלי חשוב לכיתת תכנות ארדווינו. מתכנתים נעזרים בפעולה זו על מנת לחבר שורות טקסט לצורך יצירת תוויות והודעות במסך ההצגה או למטרות אחרות.

# איך לבצע

לחבר שני תווים, ניתן להשתמש בפקודה ```+``` כדלקמן:

```Arduino
String name = "זהבה";
String greeting = "שלום, " + name + "!";
Serial.println(greeting);
```

פלט התוכנית יהיה "שלום, זהבה!".

לחבר תו אחד לסוף מחרוזת, ניתן להשתמש בפקודה ```concat``` כדלקמן:

```Arduino
String myString = "תות";
myString.concat(" בר");
Serial.println(myString);
```

פלט התוכנית יהיה "תות בר".

אם נרצה להוסיף מספרים לתחילת או הסוף מחרוזת, ניתן להשתמש בפקודה ```+=``` כדלקמן:

```Arduino
String num = "42";
String message = "המספר האהוב עליי הוא: ";
message += num;
Serial.println(message);
```

פלט התוכנית יהיה "המספר האהוב עליי הוא: 42".

ניתן גם לחבר מחרוזות רבות באופן מקוצר עם הפקודה ```+=``` כדלקמן:

```Arduino
String word1 = "קל";
String word2 = "בגבורה";
String word3 = "חומוס";
word1 += word2 += word3;
Serial.println(word1);
```

פלט התוכנית יהיה "קלבגבורהחומוס".

# נכנסים לעומקים

הפעולה של חיבור מחרוזות באמצעות ```+``` בארדווינו מבצעת בעצם פעולת קומפסציה של תווים. זה אומר שהתווים מתווספים לזה אחר זה ללא רווחים או תוויות מיוחדות. כמו כן, ניתן להוסיף רק תווים ולא פרמטרים או משתנים אחרים.

הקריאה לפקודה ```concat``` יוצרת הכללה חדשה בין התווים. כך שאם נרצה לחבר תו בלתי נר