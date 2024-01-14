---
title:                "Arduino: קריאת ארגומנטים בשורת הפקודה"
simple_title:         "קריאת ארגומנטים בשורת הפקודה"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מדוע

למה אנשים בכלל מתעניינים לקרוא ארדואינו פוסטים עבור קוד פתוח? יתכן שאתה מחפש דרך חדשה להשתמש בארדואינו, עליך לכתוב קוד שיכול לקבל פרמטרים מהפקודה העיקרית של המחשב, או שאתה סתם סקרן לגלות מה יכול לעשות ארדואינו כשמדובר בקרות צפויות.

## איך לעשות זאת

אם אתה רוצה לכתוב קוד ארדואינו שיקבל פרמטרים מהפקודה העיקרית, אתה צריך להשתמש בפונקציית "parseInt" כדי להמיר את המחרוזת שמקבלת את הפרמטר למשתנה מספרי. הנה דוגמה לכך:

```arduino
int parameter = parseInt(Serial.readStringUntil('\n'));
```

בדוגמה זו, אנחנו משתמשים בפונקציית "readStringUntil" כדי לקרוא את הפלט שלנו עד לתו השורה הבאה (כלומר, הכנסה של הפרמטר שהמשתמש זנה בשורת הפקודה) ולמידע זה יש לי את-חזוק במשתנה "parameter". כעת אנחנו יכולים להשתמש בפרמטר כלשהו שהמשתמש אינו מזהה כשמישהו לוחץ על כפתור כדי להתחיל פעולה מסוימת.

## מכהש

הפונקציונליות של קריאת ארגומנטים משולבת יותר בצורה מורכבת מאשר תמיד כמו `parseInt` מהפקודה. אתה יכול לממש את זה בצורות רפויות, לדוגמא:

```c
// this should be the first thing you define in your code
enum COMMANDS {
  // this command has no command line arguments
  NO_PARAMETERS,

  // this command has one integer argument
  PRINT_NUMBER,

  // this command has two integer arguments
  PRINT_SUM,

  // and so on. you get the idea
  CUSTOM_COMMAND
};

enum COMMANDS parseCommand(String str) {
  if (0 == str.startsWith("print sum ")) {
    return PRINT_SUM;
  } else if (0 == str.startsWith("print number ")) {
    return PRINT_NUMBER;
  } else {
    return CUSTOM_COMMAND;
  }
}

enum COMMANDS command = parseCommand(Serial.readStringUntil('\n'))
```

בעזרת הערה זו, אין אמור להיות ברור אחרת בין-לבין א