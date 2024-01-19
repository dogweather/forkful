---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

מספרים אקראיים בתכנות מתייחסים לנתונים שנוצרים לאופן חסר דוגמה ולא צפוי. מתכנתים משתמשים בהם לעיתים על מנת לבצע בדיקות עמידות, ליצירת גיוון במשחקים ולתפעול שונה של אלגוריתמים.

## איך עושים זאת:

אפשר להשתמש בפונקציה `random()` שבווינות Arduino ליצירת מספרים אקראיים.  

```Arduino
void setup() {
  Serial.begin(9600); 
  randomSeed(analogRead(0));
}

void loop() {
  Serial.println(random(100)); 
  delay(1000); 
}
```

הפונקציה `random(100)` מחזירה מספר אקראי בין 0 ל-99. `randomSeed(analogRead(0))` מאתחלים את הגנרטור האקראי.

## צלילה עמוקה 

מרכז ההוראות האקראיים הראשון נבנה בשנת 1946 ונקרא "רנדום", והפך לבסיס למספרים אקראיים של עוד הרבה שנים. ישנם שיטות אלטרנטיביות ליצירת מספרים אקראיים כמו השיטה של Monte Carlo או גנרטורים פיזיים אקראיים. אך במרבית המקרים, `random()` הוא הגישה הפשוטה והיעילה ביותר.

## ראה גם

1. Arduino Reference: Random Numbers - https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
2. How Arduino generates random numbers - https://techexplorations.com/guides/arduino/programming/random-numbers/
3. Understanding Random Numbers in Arduino - https://www.makerguides.com/arduino-random-numbers/ .