---
title:                "מציאת אורך של מחרוזת"
html_title:           "Java: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה
ישנם כמה סיבות טובות להכיר את אורך מחרוזת בקודם הכל: כדי לוודא שאתה מטפל בנתונים תקינים ולתמוך בפונקציונליות שונות כמו תיעוד וטפל בשגיאות.

## איך לעשות זאת
### קוד דוגמה
האם תמיד חשבתם על האופן שבו אתם מצויים במרחק אחד מכך להשתמש בפונקציית length של מחרוזת בכל לשון תכנות? כאן יש כמה דוגמאות שמוסברות כיצד למצוא את אורך מחרוזת עם שפת תכנות Java:

```Java
String str1 = "Hello World";
int len1 = str1.length();
System.out.println("The length of str1 is: " + len1);
```

Output:
```java
The length of str1 is: 11
```

השתמש במספר משתנים כדי לבדוק את אורך המחרוזת דרך קוד. לדוגמא:

```Java
String str2 = "שלום עולם";
int len2 = str2.length();
System.out.println("אורך מחרוזת זו הוא: " + len2);
```

Output:
```java
אורך מחרוזת זו הוא: 9
```

אם ברצונכם למצוא את אורך המחרוזת הכולל, כולל רווחים ותווים מיוחדים, ניתן להשתמש בפעולה פשוטה יותר כדי לבדוק את זה:

```Java
String str3 = "How are you?";
int len3 = str3.length();
System.out.println("המחרוזת הזו באורך של: " + len3);
```

Output:
```java
המחרוזת הזו באורך של: 12
```

עוד דרך נפוצה למצוא את אורך המחרוזת היא להשתמש בלולאה for ולעבור על התווים שבמחרוזת עד למציאת סוף המחרוזת:

```Java
String str4 = "Java Programming";
int length = 0;
for (char c : str4.toCharArray()) {
    length++;
}
System.out.println("אורך מחרוזת זו הוא: " + length);
```

Output:
```java
אורך מחרוזת זו הוא: 16
```

## Deep Dive
בתכנות, אנחנו משתמשים במחרוזת כדי לקבל קלט מהמשתמש, לעבוד עם טקסטים, ליצירת בנות אחזקה