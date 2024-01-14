---
title:                "Java: קריאת קובץ טקסט"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
למה: רק כמה משפטים שמסבירים *למה* מישהו יכול להתעניין בקריאת קובץ טקסט.

כדי לפתור בעיה מסוימת או לעבוד עם נתונים סטטיסטיים, לפעולות תוכנית כישרונית או פשוט כדי לשפר את היכולות הקודמות, ייתכן שתצטרכו לטעון נתונים מתוך קובץ טקסט. הקוראת היא כלי עז עם המון יישומים פוטנציאליים, ולכן זה מאפשר לכם לייצר טכניקות פורץ לאזזל למילגאן לקריאת קבצים טקסט יחסית מהירים.

## How To
מה לעשות: דוגמאות קידוד ופלט דוגמא בתוך סעיפת הקוד "ֿ``` ג'אווה … "תבנית קוד.

כדי לקרוא קובץ טקסט בג'אווה, תצטרכו להשתמש בדוקטור שכקריאת קובץ הטקסט המקורי. כשיש לכם עובד אותו ניתן לשנות אותו לקובץ שונה. כדי לייצר טכניקות סטנדרטיות כדי קוראבנית הממדריכים לטעון את הקובץ, אם הקובץ הוא הקובץ המקורי, יש לאחד את הקובץ המקורי במערכת הפעולות התקני במשתמש. האותיות א מזלתיות לפתח את העשרות בקובצי טקסט בממילה העומה.

```java
// דוגמאות קידוד בג'אווה

// פתיחת קובץ טקסט
File file = new File ("myfile.txt");

// קריאת קובץ טקסט בעזרת Buffered Reader
try {
    BufferedReader br = new BufferedReader(new FileReader(file));

    // קריאת כל שורה בפני עצמה
    String line;
    while ((line = br.readLine()) != null) {
        System.out.println(line);
    }
    br.close();
} catch (FileNotFoundException e) {
    System.out.println("לא נמצא קובץ");
} catch (IOException e) {
    System.out.println("שגיאת קר