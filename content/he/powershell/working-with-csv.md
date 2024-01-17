---
title:                "עבודה עם קובץ csv"
html_title:           "PowerShell: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

# עבודה עם CSV בפויז'רשל
ייתכן שתמיד נתקלנו בכתיבת קבצים המכילים מספרים ומידע מורכב, ולפעמים נטרול אותם בסיבוב ותיקונם. זאת הסיבה שבגדול תכניתאים עובדים עם קבצים מסוג CSV. בפויז'רשל, אתם יכולים לרתח תסריטים שיפעלו עם CSV, דבר המאפשר לכם לעבוד ביעילות עם קבצים משורבבים.

## מה ולמה?
עבודה עם CSV בפויז'רשל מאפשרת לכם לקרוא, לכתוב ולערוך קבצים מסוג CSV בקלות. כתיבת תסריטים בפויז'רשל היא דרך יעילה לנתח ולעבד מידע מורכב שמצוי בתוך קבצי CSV.

## כיצד לעשות זאת?
### מחיקת שורות תוך שמירת הקובץ
```
PowerShell Remove-Item -Path C:\Users\User\Desktop\file.csv
```
כאן אנחנו משתמשים בפקודה "Remove-Item" כדי למחוק קובץ מסוים מהנתיב שניתן. פירוט הקוד אמור להסביר עבורכם ברור מה עושה התסריט שלכם.

### קריאת קובץ עם כותרות
```
PowerShell Import-CSV -Header Name,Age,Gender -Path C:\Users\User\Desktop\file.csv
```
אתם יכולים ליצור רשימה של כל הכותרות שתרצו להוסיף כמו בדוגמה שלנו. הפקודה "Import-CSV" מאפשרת לכם לקרוא קובץ CSV ולפרק את המידע לפי כותרות המצויות בקובץ.

## מסתכלים מעמיק
### היסטוריית מקור
קבצי CSV נוצרו בשנות ה-70 כדי לייצג מידע מספרי בקובץ טקסט פשוט. בעזרת סימן הפסיק שמפריד בין עמודות, ניתן היה לארגן ולשמור מידע מורכב בקובץ אחד.

### אלטרנטיבות
אם אתם מעדיפים תוכניות נוספות לעבודה עם CSV, אתם יכולים להשתמש בכלי עזר כמו Excel או Google Sheets המאפשרים העלאת קבצי CSV ועיבודם באופן גרפי ומתקדם יותר.

### פרטים נוספים
בפויז'רשל, אתם יכולים לכתוב תסריטים שיתמוך בפרמטרים בהתאם לצורך ולערוך את הקובץ בצורה תואמת בדיוק לצרכיכם.

## ראו גם
- [סרטון על עבודה עם CSV בפויז'רשל](https://www.youtube.com/watch?v=aH2rCslOD3s)
- [פקודות חשובות בפויז'רשל לעבודה עם CSV](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-content?view=powershell-7.1)
- [מדריך לעבודה עם קבצי CSV בפויז'רשל](https://www.petri.com/working-csv-files-powershell)