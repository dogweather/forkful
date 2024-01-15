---
title:                "השוואת שתי תאריכים"
html_title:           "Javascript: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מדוע

ב-Javascript, ישנן מצבים מסוימים שבהם נדרש להשוות בין שני תאריכים. למשל, כאשר יש לנו תאריך התחלתי ותאריך סיום של אירוע, ואנחנו רוצים לוודא אם האירוע עבר או לא. במאמר זה, נלמד כיצד לבצע השוואת תאריכים באמצעות קוד Javascript.

## איך לעשות

השוואת תאריכים ב-JavaScript נעשה בעזרת הפעולות הבאות: `>, <, >=, <=, ===, !==` . לפניכם כמה דוגמאות לשימוש בפעולות אלה על מנת להשוות בין שני תאריכים.

קוד:
```Javascript
const date1 = new Date('2021-06-01');
const date2 = new Date('2021-06-15');
if (date1 > date2) {
  console.log('תאריך 1 אחר תאריך 2');
} else if (date1 < date2) {
  console.log('תאריך 1 קודם תאריך 2');
} else if (date1 === date2) {
  console.log('תאריך 1 שווה לתאריך 2');
}
```

פלט:
```
תאריך 1 קודם תאריך 2
```

קוד:
```Javascript
const date1 = new Date('2021-06-01');
const date2 = new Date('2021-06-15');
if (date1 >= date2) {
  console.log('תאריך 1 יותר מתאריך 2 או שווה לו');
} else if (date1 <= date2) {
  console.log('תאריך 1 פחות מתאריך 2 או שווה לו');
} else if (date1 !== date2) {
  console.log('תאריך 1 לא שווה לתאריך 2');
}
```

פלט:
```
תאריך 1 פחות מתאריך 2 או שווה לו
```

## Deep Dive

כאשר משווה בין שני תאריכים, חשוב להתחשב בכמה פרמטרים שמשפיעים על תוצאת השוואה:

1. מחשבי השעון נעים לפי הזמן המקומי של המשתמש. כלומר, אם תאריך אחד הוא בזמן מקומי שונה מתאריך השני, הפעולות הלוגיות יכולות לתת תוצאה שונה ממה שהמשתמש מצפה.
2. השוואת תאריכים משתמשת בפעולות מתמטיות. זה אומר שבמקרה של תאריכים בפורמט