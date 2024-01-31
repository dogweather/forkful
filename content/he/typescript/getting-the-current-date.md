---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:17:17.427541-07:00
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
נושא שכל תוכניתן מתמודד איתו הוא איך לשים את התאריך הנוכחי בקוד שלו. זה נחוץ למעקב אחרי זמן, תיעוד לוגים, ואימות תוקף.

## איך לעשות:
בצערת שימוש ב-Date object כדי להשיג את התאריך והזמן הנוכחיים:

```TypeScript
const now: Date = new Date();
console.log(now);
```

אם תריץ את הקוד הזה, תקבל משהו כמו:

```
2023-04-05T14:20:30.045Z
```

זה בפורמט של תאריך ושעה בזמן האוניברסלי המתואם (UTC).

## הצצה לעומק:
ה-Date object בJavaScript (ובצורתו המורחבת ב-TypeScript) אינו חידוש. JavaScript שילב תמיכה ב-Date מימיו הראשונים. TypeScript, שהוא סוג של שכבה מעל JavaScript, מוסיף מוסכמות טיפוס למושגים כאלה כמו ה-Date object.

אלטרנטיבות לשימוש ב-Date object הן ספריות חיצוניות כמו Moment.js או date-fns שמוסיפות תכונות נוספות ונוחות בעבודה עם תאריכים.

הפרטים הטכניים שמחדירים את האופן שבו TypeScript מתייחס לתאריכים נוגעים לטיפוסיות. בTypeScript, תמיכה זו מאפשרת לדייק יותר בהכרזות ובשימוש באובייקטים של תאריך, ולמנוע שגיאות בזמן קומפילציה.

## ראה גם:
- [MDN Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) - מידע על פקודת Date בJavaScript.
- [TypeScript Documentation](https://www.typescriptlang.org/docs/) - מידע רשמי על TypeScript, כולל איך להשתמש בטיפוסים.
- [date-fns Documentation](https://date-fns.org/docs/Getting-Started) - מבוא לספריית date-fns, ספריית ניהול תאריכים חזקה.
- [Moment.js Documentation](https://momentjs.com/docs/) - מדריך לספרית Moment.js לטיפול בתאריכים.
