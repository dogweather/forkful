---
title:    "C: בדיקת קיום תיקייה"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
כתיבת תעודת זהות גורם לאנשים להיות נמצאים להתקשרות עם קבצים ותיקים במערכת הפעלה. חשוב לאמוד את קיום הספרייה כדי לוודא שהקבצים נמצאים במקום הנכון ולמנוע איבוד נתונים.

## כיצד לבדוק אם ספרייה קיימת
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
	char path[] = "/home/user/folder"; // הגדרת נתיב לספרייה
	int result = system(path); // נכנס לתיקייה באמצעות הפונקציה system()
	if (result == 0) { // בדיקת ערך החזרה
		printf("%s is existing", path); // אם ערך הפונקציה הוא 0, המערכת מדליקה שהנתיב קיים
	}
	else {
		printf("%s is not existing", path); // אם ערך הפונקציה הוא שונה מ0, המערכת דרסה שהנתיב אינו קיים
	}
	return 0;
}
```
### פלט:
```
/home/user/folder is existing
```

### נכנסים לפרק העמוק
בנוסף לבדיקת קיום ספרייה באמצעות הפונקציה system(), ניתן להשתמש גם בפונקציות מובנות כגון stat(), access() וכו'. הפונקציות הללו עוזרות לבדוק את האטריביוטים של התיקייה כגון הרשאות קריאה וכתיבה, גודל התיקייה, תאריך יצירת התיקייה וכו'.

## ראה גם
- [כתיבת תעודת זהות עם שפת סי](https://he.wikipedia.org/wiki/%D7%AA%D7%A2%D7%95%D7%93%D7%AA_%D7%96%D7%94%D7%95%D7%AA_%D7%A1%D7%99)
- [פונקציות מובנות לניהול קבצים בשפת סי](https://www.geeksforgeeks.org/built-function-c/)
- [מדריך לכתיבת תעודת זהות בספריית הסטנדרטית בשפת סי](https://www.tutorialspoint.com/c_standard_library/c_function_system.htm)