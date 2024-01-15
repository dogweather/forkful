---
title:                "המרת תאריך למחרוזת"
html_title:           "Javascript: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

אם אתם מפתחי תוכנה או מתעסקים עם דוחות ונתונים שיש להם תאריכים, כנראה שתפגשו בצורך להמיר את התאריך למחרוזת. המטרה של המאמר הזה היא ללמד אתכם כיצד להתמודד עם זווית זו של מתמטיקה תאריכים ממש תכנוכת מרכב שפת ג'אווה-סקריפט

## איך לעשות זאת

המרה של תאריך למחרוזת בג'אווה-סקריפט היא לא פשוטה, אך עדיין מבוצעת בקלות כאשר נמצא את הדרך הנכונה לעשות זאת.

תחילה, נצטרך למצוא את התאריך שאנו רוצים להמיר למחרוזת. כדי לעשות זאת, נשתמש בפונקציה המובנת "new Date()" ונעביר אליה את התאריך שאנו רוצים להמיר כפרמטר. לדוגמה:

```Javascript
let date = new Date("June 18, 2021");
```

כעת, נשתמש במתודה "toString()" כדי להמיר את התאריך למחרוזת:

```Javascript
let stringDate = date.toString();
```

לכבוד הדוגמה, נניח שהתאריך שאנו המירנו הוא יום שישי, יוני 18, 2021. הפלט של המחרוזת הוא:

```Javascript
"Fri Jun 18 2021 00:00:00 GMT+0300 (Israel Standard Time)"
```

כפי שאתם רואים, המחרוזת כוללת את התאריך המלא עם יום השבוע, תאריך, שעה ושעון עם האזור הזמנים שלנו.

אם ברצונכם לקבל את התאריך בתבנית שונה, יש לכם אפשרות להשתמש במתודה "toLocaleDateString()". נשתמש בדוגמה נוספת כדי להמחיש את כך:

```Javascript
let date = new Date("June 18, 2021");
let stringDate = date.toLocaleDateString("en-US", { 
  year: "numeric", 
  month: "short",
  day: "numeric" 
});
```

בדוגמה זו, נקבל את התאריך בפורמט שנבחר, כאשר הפלט של המחרוזת הוא:

```Javascript
"June 18, 2021"
```

כפ