---
title:    "Java: חילוץ חתיכות מחרוזת"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

למה כדאי לדוברי העברית ללמוד כיצד לחלץ תת-מחרוזות ב-Java? כי חלץ תת-מחרוזות הוא כלי חשוב בשפת תכנות זו ומאפשר לנו לטפל במחרוזות מסובכות בצורה יעילה ויצירתית.

## כיצד

תחילה, נגדיר מחרוזת עיקרית עם השם "str". לאחר מכן, נשתמש בפקודות המתאימות כדי לחלץ תת-מחרוזות מ-"str". לדוגמה, אם נרצה לחלץ את התת-מחרוזת "Java" מהמחרוזת העיקרית "Welcome to Java programming", נשתמש בפקודה str.substring(11, 15). המספרים 11 ו-15 מציינים את המיקום של התווים במחרוזת העיקרית מהם נרצה לחלץ את התת-מחרוזת.

```Java
String str = "Welcome to Java programming";
System.out.println(str.substring(11, 15));
// Output: Java
```

בנוסף, ניתן להשתמש בפקודת str.indexOf("Java") כדי למצוא את המיקום הראשון של המחרוזת "Java" במחרוזת העיקרית ולשים אותו כמוצא לפקודת str.substring().

```Java
String str = "Welcome to Java programming";
int start = str.indexOf("Java");
System.out.println(str.substring(start, start+4));
// Output: Java
```

לנוחיותכם, ניתן לכתוב פקודת חלץ תת-מחרוזת כמתודה ולהשתמש בה כמקור עבור יותר מחרוזת אחת.

```Java
public static String substring(String mainStr, String subStr) {
    int start = mainStr.indexOf(subStr);
    int end = start + subStr.length();
    return mainStr.substring(start, end);
}

String str = "Welcome to Java programming";
String subStr = "Java";
System.out.println(substring(str, subStr));
// Output: Java
```

## חפירה עמוקה

חלץ תת-מחרוזת הוא פעולה חשובה בעיבוד מחרוזות בשפת תכנות Java. המאמר הזה הסביר כיצד לממש את פקודת החלץ בצורה פשוטה וברורה, אך חשוב להיזהר מאיזושהי בעיות אפשריות. לדוגמה, אם התיחסות למיקום התת-מחרוזת במקור העיקרי תיעשה ללא עיוותים, כדי ל