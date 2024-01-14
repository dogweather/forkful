---
title:                "TypeScript: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה:

למה להתעסק עם קבלת התאריך הנוכחי? המבקרים שלנו, כמוני, יכולים לשאול את עצמם. אז, למה? התאריך הנוכחי הוא מידע חשוב ביותר בכל פרויקט תכנותי וקשה לדמיין פרויקט איכותי שלא משתמש בתאריך הנוכחי. זה משמעותי גם למנהלים, מפתחים ומשתמשים כאחד, כי זה מאפשר להבין מתי מאמר או אפליקציה נוצרו ולגזור מצב כשהתאריך היה כאשר בא להשוות בין שונים גרסאות או עדכונים.

## איך לקבל את התאריך הנוכחי:

התקנו טיפוסקריפט במחשב שלכם (אם עוד לא בברירת מחדל), ואז כתבו את הקוד הבא בתיקייה שלכם:

```TypeScript
const currentDate = new Date();
console.log(currentDate);
```

ברגע שתפעילו את הקוד הזה, תראו את התאריך הנוכחי מודפס בקונסול.

תוכלו גם להתאים את הפורמט של התאריך לפי הצורך. למשל, תוכלו להשתמש בפיסת קוד הבאה כדי להדפיס את התאריך בפורמט DD/MM/YYYY:

```TypeScript
const currentDate = new Date();
console.log(`${currentDate.getDate()}/${currentDate.getMonth()+1}/${currentDate.getFullYear()}`);
```

כמו כן, תמיד כדאי להבין טוב יותר את הפונקציונליות של התאריך הנוכחי. למשל, נתקלנו במקרה שבו נדרש לשמור תאריך ושעה, ולכן השתמשנו בקוד הבא:

```TypeScript
const currentDateTime = new Date().toLocaleString();
console.log(currentDateTime);
```

כדי למצוא עוד מידע על כיצד לקבל את התאריך הנוכחי ולהתאים אותו לפי הצורך שלכם, ניתן לקרוא את המדריך המפורט של מיקרוסופט כאן.

## העומק שבקבלת התאריך הנוכחי:

כמו שאנו רואים, קבלת התאריך הנוכחי י