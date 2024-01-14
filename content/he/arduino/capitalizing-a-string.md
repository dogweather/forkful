---
title:    "Arduino: שינוי אותיות לגדלות במחרוזת"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

כאשר מחליטים לבנות פרויקט עם ארדואינו ייתכן שנתקלו בצורך לרשום מחרוזת (String) כלשהי, וייתכן שרוצים לכתוב אותה כבקשר (Capitalized). פעולה זו תכול לתרום לקביעה שאתם מנסים להעביר בכתובת אתרים כלשו. 

## איך לבצע זאת

אם אתם מעוניינים לבנות פונקציה שתכתוב לנו מחרוזת כקביעה תוכלו לעשות זאת בקלות באמצעות תוסף שמקבל שלשה פרמטרים. החלונית שני הפרמטרים יכולים להיות מחרוזות ולמהדת את הטקטס לצורך כתיבה מחרוזת עם האות הגדולה אשר תהיה גם כזה של קביעה.

```Arduino
String capitalize(String str, String separator) {
    String result = "";
    for (uint8_t i = 0; i < strlen(str.c_str()); i++) {
        char c = str.charAt(i);
        if (i == 0 || str.charAt(i - 1) == separator[separator.length() - 1] || (str.charAt(i - 1) == '-' && separator[0] == '-')) {
            if (c >= 'a' && c <= 'z') {
                c += 'A' - 'a';
            }
        }
        result.concat(c);
    }
    return result;
}
```

הנה מספר לוגים במטריית התוכנית:

```Arduino
"separate", "", "Capitalize only first char: " => Capitalize_only_first_char
"The-Second", "-", "Capitalize first char of each word: " => Capitalize_First_Char_Of_Each_Word
```

הפלט של התוכנית יופיע ככה:

```Arduino
Capitalize only first char: Separate
Capitalize first char of each word: The Second
```

## חקירה מעמיקה

הפונקציה הנ"ל מבוססת על הרצונות למצוא את האות הראשונה של כל מילה במחרוזת.


## ראה גם

- [מפרט על ארדווינו](https://www.arduino.cc/en/guide/introduction)
- [הפיכת מחרוזת לקביעה בפייתון](https://www.geeksforgeeks.org/convert-string-to-camel-case-in-python/)