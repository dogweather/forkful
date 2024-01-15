---
title:                "שרשור מחרוזות"
html_title:           "TypeScript: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

למה לאדם לעסוק באיחוד מחרוזות? פשוט, כל תוכנית בתכנות משתמשת במחרוזות באופן רחב, לכן חשוב שנדע איך לחבר אותן בצורה נכונה כדי ליצור תוצאה תקינה.

## איך ל

על מנת לחבר מחרוזות ב TypeScript, ניתן להשתמש באופרטור הכפל ( + ). לדוגמה:

```TypeScript
let firstName: string = "טלי";
let lastName: string = "כהן";

console.log(firstName + " " + lastName);

// תוצאה: טלי כהן
```

## חקירת עומק

האם אתם יודעים שניתן לחבר מחרוזות ב TypeScript עם האופרטור +=? בנוסף, ניתן גם להשתמש בפונקציה concat() כדי לחבר מספר מחרוזות במקביל. יתרה מכך, אם מצביעים על מחרוזת בתוך שורת קוד, ניתן להשתמש בפונקציה toString() כדי להמיר את המצביע למחרוזת לפני שמחברים אותה עם מחרוזת אחרת.

## ראו גם

- [JavaScript מדריך תחבירי: מחרוזות](https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Grammar_and_types#strings_in_javascript)
- [מחרוזות ב TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- [השתמש במחרוזות ב TypeScript כמו מקצוען](https://www.digitalocean.com/community/tutorials/how-to-use-strings-in-typescript-like-a-professional)