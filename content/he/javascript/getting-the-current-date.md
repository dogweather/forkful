---
title:                "Javascript: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה
כיוון שטכנולוגיות המחשב מתקדמות ומתעדכנות במהירות, ישנם מספר סיבות שבהן נצטרך להשתמש בתאריך נוכחי בתוך קוד ג'אווהסקריפט. למשל, להצגת תאריך באופן דינמי באתר אינטרנט, ליצירת רשימת תאריכים או לצורך תיעוד של פעולות מסוימות.

# איך לעשות זאת
לפניכם ישנם שני אפשרויות לקבלת התאריך הנוכחי בתוך קוד ג'אווהסקריפט. הפעולה הראשונה היא להשתמש בפונקציה המובנית `Date()` שמחזירה את התאריך והשעה של הקלט הנוכחי מהמחשב שלכם. הנה דוגמה לשימוש בפונקציה זו ולהדפסת התאריך בפורמט של חודש, יום ושנה:

```javascript
var currentDate = new Date();
var month = currentDate.getMonth();
var day = currentDate.getDate();
var year = currentDate.getFullYear();

console.log("The current date is: " + month + "/" + day + "/" + year);
```

פרט חשוב לציין הוא שערכי החודש והיום יחסיים בפונקציה `getMonth()`, כלומר יש להוסיף 1 לקבלת ערך החודש הנכון. כמו כן, יש לציין שתאריך השנה הנמוך מכל הפונקציות בין השימוש בפונקציה `getFullYear()`.

האפשרות השנייה היא להשתמש בספרייה חיצונית כגון [Moment.js](https://momentjs.com/) שמספקת יכולות עבור תלתליות, תיעוד וניתוח תאריכים. ניתן להתקין את הספרייה באמצעות פקודת ההתקנה `npm install moment` ולהשתמש בה בקוד כדם:

```javascript
var moment = require('moment');
var currentDate = moment().format("DD/MM/YYYY");

console.log("The current date is: " + currentDate);
```

ניתן להתאים את הפורמט של התאריך לפי הצורך שלכם. לדוגמה, להחזיק את השנה בפורמט שני ספרות א