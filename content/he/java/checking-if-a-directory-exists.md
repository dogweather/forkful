---
title:                "Java: האם תיקייה קיימת"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
למה לבדוק אם תיקייה קיימת - לפני כל פעולה שלנו עם תיקיות בתוך קוד ג'אווה, חשוב לוודא שהיא קיימת כדי לחסוך באגים ולהבטיח שהקוד ירוץ בצורה תקינה.

## איך לעשות זאת
```Java
import java.io.File;

public class CheckDirectoryExists {

  public static void main(String[] args) {
    // יצירת אובייקט תיקייה חדשה
    File directory = new File("תיקייה חדשה");

    // בדיקה אם התיקייה קיימת כבר
    if (directory.exists()) {
      System.out.println("התיקייה כבר קיימת!");
    } else {
      // אם התיקייה לא קיימת, ניתן ליצור אותה בצורה אוטומטית
      if (directory.mkdir()) {
        System.out.println("התיקייה נוצרה בהצלחה!");
      } else {
        System.out.println("לא ניתן ליצור את התיקייה.");
      }
    }
  }
}
```

### פלט
אם התיקייה כבר קיימת:
```
התיקייה כבר קיימת!
```

אם התיקייה לא קיימת וניתן ליצור אותה:
```
התיקייה נוצרה בהצלחה!
```

אם התיקייה לא קיימת ולא ניתן ליצור אותה:
```
לא ניתן ליצור את התיקייה.
```

## Deep Dive
כדי לבדוק אם תיקייה קיימת בקוד ג'אווה, אנו משתמשים במחלקת File. למחלקה זו יש מספר שיטות שנועדו לבדוק אם תיקייה או קובץ קיים או לא. שיטת "exist()" מחזירה ערך בוליאני - true אם התיקייה קיימת וfalse אם היא לא קיימת. בנוסף, ניתן גם להשתמש בשיטת "isDirectory()", שמחזירה ערך בוליאני ומאמת אם האובייקט הם file הוא באמת תיקייה.

## See Also
* מדריך לעבודה עם קבצים ותיקיות בג'אווה: https://www.w3schools.com/java/java_files.asp
* המחלקה File ב-Java: https://www.geeksforgeeks.org/file-class-in-java/
* בדיקת תיקיות וקבצים קיימים בג'אווה: https://www.callicoder.com/java-check