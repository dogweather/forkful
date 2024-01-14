---
title:    "Java: לקבל את התאריך הנוכחי"
keywords: ["Java"]
---

{{< edit_this_page >}}

# למה:

תאר לעצמך את המצב הבא - אתה מפתח אפליקציה מורכבת המצריכה להציג את התאריך הנוכחי. מה עושים? אם אי פעם נתקלתם במצב דומה ואתם מתבלבלים למה צריך תמיד להציג את התאריך הנוכחי באפלקציות? כדי לסייע לכם בפתרון שאלה זו, ישנו כאן הפתרון האידיאלי שיגיע להציל את המצב ויעניק לכם כלי מקיף ואינטואיטיבי לעבוד עם תאריכים בקלות ובפשטות.

## איך לעשות:

כדי לקבל את התאריך הנוכחי בקוד הפתקים שלכם, ניתן להשתמש במחלקת ```LocalDate``` המובנית בספריית הקוד הפתוח של Java. על מנת להשתמש במחלקה זו, נצטרך לייבא אותה בשורת הקוד הראשונה של הקוד שלנו:

```java
import java.time.LocalDate;
```

לאחר מכן, ניתן ליצור משתנה מסוג ```LocalDate``` באמצעות הפעלת הפעולה ```now()```, שמציג את התאריך הנוכחי באופן אוטומטי:

```java
LocalDate currentDate = LocalDate.now();
```

כעת, ניתן להשתמש במשתנה הזה באמצעות הפעלת פעולות כמו ```getDayOfMonth()```, ```getMonthValue()```, וכו ', לפי הצורך באפליקציה שלנו.

לדוגמה, אם נרצה להציג את התאריך הנוכחי בפורמט מסודר ומובן יותר, נוכל להשתמש בפעולה ```format()``` ולהעביר לה את הפורמט הרצוי כמו המשתנה המסוג ```DateTimeFormatter```:

```java
// ייבוא המחלקה DateTimeFormatter מתוך הספריית הקוד הפתוח של Java
import java.time.format.DateTimeFormatter;

// יצירת משתנה מסוג DateTimeFormatter והגדרת הפורמט שלו
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

// השתמשות בפעולת הפורמט על המשתנה currentDate והדפסה של הת