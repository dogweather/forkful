---
title:                "Java: המרת מחרוזת לאותיות קטנות"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Hebrew Translation:

## למה
מדוע אדם ימלאך להמיר מחרוזת לאותיות קטנות?

## כיצד לבצע זאת
לדוגמה ופלט בתוך בלוקי קוד "```Java ... ```"

כאן תוכלו למצוא כמה דרכים להמיר מחרוזת לאותיות קטנות בשפת ג'אווה:

```Java
// דרך ראשונה: להשתמש בפונקציית toLowerCase()
String str = "ג'אווה";
String strLower = str.toLowerCase();
System.out.println(strLower); // פלט: ג'אווה

// דרך שנייה: להשתמש בלולאת פוריצ'
String str = "ג'אווה";
StringBuilder strLower = new StringBuilder();
for(int i=0; i<str.length(); i++){
    char c = str.charAt(i);
    if(Character.isUpperCase(c)){
        c = Character.toLowerCase(c);
    }
    strLower.append(c);
}
System.out.println(strLower); // פלט: ג'אווה

// דרך שלישית: להשתמש במתודה של String שממירה את התווים לקודי ASCII ולהוסיף 32 יחידות
String str = "ג'אווה";
char[] chars = str.toCharArray();
for(int i=0; i<chars.length; i++){
    if(chars[i] > 64 && chars[i] < 91){
        chars[i] += 32;
    }
}
str = String.valueOf(chars);
System.out.println(str); // פלט: ג'אווה
```

## העומק המתמטי
כאשר מופעלת המתודה toLowerCase() במחרוזת, הרכיבים שלה נמחקים ומאוחסנים מחדש בתור אותיות קטנות במערך תווים חדש. זה משפיע על זיכרון המחשב ועל ביצועי התוכנה, אך בכמה מקרים יכול להיות עדיף להשתמש באחת מן הדרכים הנ"ל שתוצר תוצאה דומה בצורה יעילה יותר.

## ראו גם
- דרכים נוספות להפעיל פונקציית toLowerCase(): https://www.geeksforgeeks.org/converting-roman-numerals-decimal-lying-1-3999/
- דרך העברת מחרוזות ל-ASCII בג'אווה: https://stackoverflow.com/questions/5509025/how-do-i-convert-a-string-to-ascii-in-java
- פונקציות חשבון וסימנים קיצורי דרך של ה-ASCII: https://www.rapidtables.com/code/text/ascii-table.html