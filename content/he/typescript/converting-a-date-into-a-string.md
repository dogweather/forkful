---
title:                "TypeScript: המרת תאריך למחרת"
simple_title:         "המרת תאריך למחרת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# למה

המרת תאריך למחרוזת בתוך קוד טייפסקריפט יכולה להיות חשובה לצורך הצגת מידע בפורמט המתאים להצגה למשתמש. כשמתעסקים עם תאריכים בתוך קוד מחשב, ייתכן שנצטרך להציג אותם כמחרוזת בתוך עיצוב של האתר או האפליקציה שאנו כותבים. במאמר זה נלמד איך להמיר תאריך למחרוזת בקוד טייפסקריפט.

## איך לעשות זאת

תחילה, ניצור משתנה שמכיל תאריך כלשהו בתוך קוד טייפסקריפט. לדוגמה, ניקח את התאריך הנוכחי ונשמיט ממנו את השעה והדקות, כך שנקבל תאריך בלבד:

```TypeScript
let date = new Date();
date.setHours(0, 0, 0, 0); // שמירה רק על התאריך, מחיקת שעה ודקות
```

כעת, נשתמש במתודה `toLocaleDateString()` כדי להמיר את התאריך למחרוזת. נעביר לפונקציה את השפה המתאימה ואת אפשרויות הפורמט שאנחנו רוצים:

```TypeScript
date.toLocaleDateString("he-IL", { year: 'numeric', month: 'long', day: 'numeric' }); // פורמט של תאריך בעברית, למשל "4 באוגוסט 2021"
```

אם נרצה להציג את התאריך בפורמט קצר יותר, נוכל להשתמש במתודה `toLocaleString()` ולהגדיר רק את מה שאנחנו רוצים להציג:

```TypeScript
date.toLocaleString("he-IL", { month: '2-digit', day: '2-digit' }); // תאריך בפורמט הקצר של חודש/יום, למשל "04/08"
```

## צלילה עמוקה

השתמשנו במתודות `toLocaleDateString()` ו-`toLocaleString()` כדי להמיר תאריך למחרוזת בקוד טייפסקריפט. אם נרצה להציג גם את הזמן, נוכל להשתמש במתודות `toLocaleTimeString()` ו-`toLocaleString()` באופן דומה.

כעת, נלמד עוד