---
title:                "Java: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד בג'אווה עשוי להיות מסע מרתק, אבל ישנן פעולות בסיסיות שהן חיוניות לכל תכנית. אחת מכך היא המרת מחרוזת לאותיות קטנות. לא תמיד אנחנו נכתוב קוד שמורכב מספר רב של תווים גדולים וקטנים, אבל במקרה כזה, יתכן ונרצה להשתמש בכלי שיגרום לכל התווים להיות בקלות. במדעי המחשב קוראים לזה "תחביר כללי". בכתבה הזאת, אנחנו נכיר את המעטפת String המבצעת פעולת המרת לאותיות קטנות.

## איך לעשות

בקוד ג'אווה, נוכל להשתמש במחלקת String המכילה פעולות שימושיות לעבודה על מחרוזות. אחת מהן היא הפעולה `toLowerCase()` שמבצעת מרת מחרוזת לאותיות קטנות. ננסה זאת על מחרוזת פתוחה באמצעות הפעולה הבאה:

```Java
String example = "HELLO WORLD";
System.out.println(example.toLowerCase());
```

תוצאה:

```
hello world
```

כמו כן, אם נרצה להשתמש בפעולה על מחרוזות שונות ניתן לכתוב קוד כמו הבא:

```Java
String firstString = "cOmpUTEr ScIeNcE";
String secondString = "eNdLESS LearNiNG";

System.out.println(firstString.toLowerCase());
System.out.println(secondString.toLowerCase());
```

תוצאה:

```
computer science
endless learning
```

אם נרצה להשתמש בתחביר כללי, ניתן להשתמש בפעולה על משתנה המכיל מחרוזת כפי שמוצג בקוד הבא:

```Java
String phrase = "I LoVE CoDINg";
System.out.println(phrase.toLowerCase());
```

תוצאה:

```
i love coding
```

ניתן גם ליצור מספר משתנים ולהשתמש בפעולת המרה כדי לקבל את התוצאה הרצויה:

```Java
String firstName = "joHN";
String lastName = "sMITh";
String fullName = firstName + " " + lastName;

System.out.println(fullName.toLowerCase());
```

תוצאה:

```
john smith
```

## חקר עמוק

כאשר אנחנו משתמשים בפעולת המרה לאותיות קטנ