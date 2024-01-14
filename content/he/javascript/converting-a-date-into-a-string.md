---
title:    "Javascript: המרת תאריך למחרוזת"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

למה: למה להעסיק בהמרת תאריך לתו מחרוזת? כי תרצו להציג את התאריך בפורמט מסוים או לשלב תאריך ושעה בתו מחרוזת בפעולות שלכם.

איך לעשות זאת: באמצעות מספר פעולות פשוטות בקוד ג'אווהסקריפט, נוכל להמיר תאריך למחרוזת ולהציג אותו בפורמט הרצוי. נצטרף לכל ערכת כלים לתמיכה בתאריכים כמו תיבת טקסט וספריית Moment.js כדי לקבל תוצאות מדויקות יותר.

```Javascript

// יצירת אובייקט תאריך חדש
const today = new Date();

// שימוש בפעולות getDate ו-getMonth כדי להחזיר את היום, החודש והשנה של התאריך
const day = today.getDate(); // כמה ימים בחודש: 1 - 31
const month = today.getMonth(); // על פי מילון: 0 - ינואר, 11 - דצמבר
const year = today.getFullYear(); // שנה מלאה: YYYY

// הצגת התאריך בפורמט "DD/MM/YYYY"
console.log(`${day}/${month + 1}/${year}`);

// הצגת הזמן של התאריך בפורמט "HH:MM"
const hour = today.getHours(); // שעה: 0 - 23
const minute = today.getMinutes(); // דקות: 0 - 59
console.log(`${hour}:${minute}`);
```

עומק הדיון: המרת תאריך למחרוזת לא תמיד נמצאת בתוך הקוד השליט, במיוחד כאשר משתמשים בתיבת טקסט להצגת תאריך. אם אתם משתמשים בזמן של ג'אווהסקריפט להצגת תאריך, ייתכן שתתקלו בבעיות במסך או תצטרכו לבצע כמה חישובים כדי לקבל את הפורמט הנכון. ניתן להיעזר בספריית Moment.js כדי לפשט את התהליך ולקבל תאריך מדויק ונוח להצגה.

ראו גם:

- מדריך לפעולות תאריך בג'אווהסקריפט: https://www.w3schools.com/js/js_date_methods.asp
- ספריית Moment.js: https