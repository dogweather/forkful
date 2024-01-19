---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת היא פעולה בקוד הבודקת דרך מזהה (Path) אם ספרייה מסוימת קיימת או לא. מתכנתים עושים את זה כדי למנוע שגיאות בהמשך, כשהם מניחים שהספרייה כבר קיימת.

## איך עובדים עם זה:
```Java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        Path path = Paths.get("your_directory_path_here");

        if (Files.exists(path)) {
            System.out.println("The directory exists.");
        } else {
            System.out.println("The directory does not exist.");
        }
    }
}
```
פלט דוגמה:
```
The directory exists.
```
או
```
The directory does not exist.
```

## צלילה עמוקה:
בקוד פתוח של Java מהדורת 7 ומעלה, פונקציית `Files.exists()` מוצגת, שמאפשרת לך לבדוק אם הנתיב (Path) שצוין קיים או לא. בעבר, פניה למערכת ההפעלה היתה נדרשת בשביל זה.
חלופות ל `Files.exists()` יכולות להיות `File.exists()` או `Files.notExists()`.
פרטי אימפלמנטציה: הפונקציה בודקת את הנתיב שנמסר לה, ואם הנתיב כתובת (כתובת הדיסק של הקובץ) היא לספרייה, היא מחזירה true.

## ראה גם:
* [Java - Files exists() Method](https://www.tutorialspoint.com/java_nio/java_nio_files_exists.htm)
* [Path Operations (The Java™ Tutorials > Essential Classes > Basic I/O)](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)
* [Java check if file exists](https://www.journaldev.com/861/java-check-if-file-exists)