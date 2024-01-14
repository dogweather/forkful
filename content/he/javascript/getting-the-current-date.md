---
title:    "Javascript: לקבלת התאריך הנוכחי - כותרת המאמר על תכנות מחשב"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# למה

יחד עם אתרי האינטרנט והיישומים המודרניים, תאריכו הנוכחי משמש ככלי מאוד חשוב בתכנות ג'אווהסקריפט. בכתבה זו נלמד למה אנו צריכים להשתמש בתאריך הנוכחי וכיצד לבצע זאת בקלות בקוד ג'אווהסקריפט.

## כיצד לבצע זאת

כדי לקבל את התאריך הנוכחי בג'אווהסקריפט, נשתמש בתוכנית המובנית Date(). ניתן לשלוח לפעולה זו פרמטרים כדי לקבל תאריך ספציפי, אך אם לא נציין כלום נקבל את התאריך הנוכחי בפורמט של תאריך מלא.

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

הפלט של הקוד הנ"ל יהיה תאריך מלא בפורמט של יום, חודש ושנה. ניתן גם להציג רק חלקים ספציפיים של התאריך כגון שנה או חודש.

```Javascript
// הצגת שנה
let currentYear = currentDate.getFullYear();
console.log(currentYear);

// הצגת חודש
let currentMonth = currentDate.getMonth();
console.log(currentMonth);
```

בנוסף, ניתן להשתמש בתצורות שונות של התאריך הנוכחי כמו תאריך חיובי או שלישותיות.

```Javascript
// תאריך חיובי
let positiveDate = currentDate.setDate(currentDate.getDate() + 7);
console.log(positiveDate);

// תאריך שלישיותי
let fractionalDate = currentDate.setDate(currentDate.getDate() + 0.5);
console.log(fractionalDate);
```

## ייעולים נוספים

למרבה המזל, ניתן גם להשתמש בספריות נוספות בג'אווהסקריפט כדי לעבוד עם תאריכים בצורה יותר מתקדמת ונוחה. ספריות אלו כוללות את Moment.js ו-Date-fns.

# רכיבים נוספים

- תאריך ושעה בג'אווהסקריפט: https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js ספריית תאריך ושעה: https://momentjs.com/
- Date-fns ספריית תאריך ושעה