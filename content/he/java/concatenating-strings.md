---
title:                "Java: לחיבור מחרוזות"
simple_title:         "לחיבור מחרוזות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

טכניקת חיבור מחרוזות היא חלק חשוב בתכנות ג'אווה שימושי לייצוג טקסט. היא מאפשרת למשתמש לדבר בצורה נוחה וזה לא רק תוחלת ללא פשרות במוח שלך. בכתיבת קוד, אנחנו לעיתים קרובות נתקלים במצבים שבהם אנחנו צריכים לחבר כמה מחרוזות יחד וזאת בדיוק מה שאנחנו עושים באמצעות טכניקה זו.

## איך לעשות זאת

חיבור מחרוזות בג'אווה נעשה באמצעות אופרטור + או באמצעות פעולת חיבור ישירה עם המחרוזות. נציג כמה מקרים כדי להבין טוב יותר איך לעשות זאת:

```Java
String str1 = "שלום";
String str2 = "עולם";
String result = str1 + str2;

System.out.println(result);
```

פלט: שלוםעולם

או באמצעות תסריט קצר יותר:

```Java
String str1 = "שלום";
String result = str1 + " עולם";

System.out.println(result);
```

פלט: שלום עולם

ניתן גם לחבר מספר מחרוזות בפעולה אחת:

```Java
String str1 = "שלום";
String str2 = " עתה ";
String str3 = "תיכף";
String result = str1 + str2 + str3;

System.out.println(result);
```

פלט: שלום עתה תיכף

משהו מעניין לשים לב הוא שהטכניקה גם עובדת עם מספרים:

```Java
int num1 = 10;
int num2 = 20;
String result = "סכום המספרים הוא: " + (num1 + num2);

System.out.println(result);
```

פלט: סכום המספרים הוא: 30

## חקירה מעמיקה

כעת שאנחנו יודעים איך לחבר מחרוזות בג'אווה, נתבונן לעומק כדי להבין כיצד פעולת החיבור מתבצעת. כאשר אנחנו משתמשים באופרטור + כדי לחבר מחרוזות, מאחורי הקלעים מתבצעת פעולת המרה והמחרוזות מתחברות בצורה חדשה. בד