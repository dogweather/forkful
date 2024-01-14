---
title:    "Java: חיבור מחרוזות"
keywords: ["Java"]
---

{{< edit_this_page >}}

## למה
איך הקינקותנות היא חלק חשוב ובלתי נפרד מתהליך התכנות בג'אווה. השימוש בקינקותנה מאפשר לנו לקבץ יחד מחרוזות שונות וליצור סדרה ייחודית שתכליתה לבצע פעולות ולהציג תוצאות בתוכניות שלנו.

## איך לעשות זאת
כדי להתחיל לקינקט מחרוזות בג'אווה, ניתן להשתמש בפקודה `+` בין כל שתי מחרוזות שברצוננו לקינקט. לדוגמה:
```Java
String firstName = "דניאל";
String lastName = "כהן";
String fullName = firstName + " " + lastName;
System.out.println(fullName);
```
שאלי: דניאל כהן

ניתן גם להשתמש במתודה `concat()` כדי לקינקט מחרוזות בג'אווה:
```Java
String weapon1 = "חרב";
String weapon2 = "קש";
String weapon3 = weapon1.concat(weapon2);
System.out.println(weapon3);
```
שתלי: "חרב קש"

## מעמקים
בנוסף לשימוש הפשוט של הפקודה `+` והמתודה `concat()` כדי לקינקט מחרוזות, ישנם מספר שימושים מתקדמים יותר של קינקותנות בג'אווה. אחד מהם הוא קינקותנה עם מספר משתנים. ניתן לבצע קינקותנה של מספר מחרוזות ומשתנים במקביל, באמצעות הפקודה `+`. לדוגמה:
```Java
String firstName = "דניאל";
String lastName = "כהן";
int age = 25;
String message = "שלום, קוראים לי " + firstName + " " + lastName + " ואני בן " + age + " שנים.";
System.out.println(message);
```
שלום, קוראים לי דניאל כהן ואני בן 25 שנים.

עוד נקנון מתקדמת היא שימוש במתודת `format()` של מחלקת `String`. המתודה זו מאפשרת לנו לבצע קינקומיד דינמי עבור מחרוזת עם משתנים שונים. ניתן להשתמש במתודה זו באמצעות `String.format()` או `System.out.prinft()` להדפסת התוצאה. לדוגמ