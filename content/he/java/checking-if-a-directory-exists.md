---
title:    "Java: בדיקת קיום תיקייה בתוכניות מחשוב"
keywords: ["Java"]
---

{{< edit_this_page >}}

## למה

בכל פרויקט פיתוח תוכנה יש הצורך לבדוק אם קיים תיקייה מסוימת במערכת הקבצים. זה יכול להיות חלק מהשלב האיתור והתיקון של באגים, או להגביל גישה לקבצים כדי לשמור על בטיחות ופרטיות. במאמר הזה נכיר את הדרכים השונות לבדוק אם תיקייה קיימת ב-Java ואת הסיבות לכך.

## כיצד לבדוק תיקייה קיימת ב-Java

הדרך הפשוטה ביותר לבדוק אם תיקייה קיימת ב-Java היא להשתמש בפונקציה `exists()` של אובייקט הקובץ, ולתת לה את הנתיב המלא של התיקייה. אם הפונקציה מחזירה אמת, זאת אומרת שהתיקייה קיימת.

```Java
File directory = new File("C:/Users/UserName/Documents/Project");
boolean exists = directory.exists();
```

אם ברצונכם לבדוק אם התיקייה קיימת בתוך תיקייה אחרת, תוכלו להשתמש בפונקציה `isDirectory()` על התוצאה של `exists()`.

```Java
File directory = new File("C:/Users/UserName/Documents/Project");
if (directory.exists() && directory.isDirectory()) {
  System.out.println("The directory exists.");
}
```

אם תרצו להיות מדויקים יותר, תוכלו להשתמש בפונקציה `getAbsolutePath()` כדי לקבל את הנתיב המלא של התיקייה ולוודא שהוא תואם לנתיב שמתמצאים בו.

```Java
File directory = new File("C:/Users/UserName/Documents/Project");
File absDirectory = new File(directory.getAbsolutePath());
if (absDirectory.exists() && absDirectory.getAbsolutePath().equals(directory.getAbsolutePath())) {
  System.out.println("The directory exists.");
}
```

## Deep Dive

בהרבה מקרים, הבדיקה הפשוטה יותר של `exists()` תספיק. אך אם ברצונכם להעמיק יותר בנושא, ישנם כמה מצבים אותם כדאי לקחת בחשבון:

- אם התיקייה לא קיימת, `exists()` תחזיר שקר. אם במקרה זה אתם מנסים לגשת לתיקייה, תקבלו שגיאה. כדאי לחבר לבדיקת `exists()` גם את `canRead