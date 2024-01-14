---
title:    "TypeScript: השוואת שתי תאריכים"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

מיכללה, כאשר אנו כותבים קוד, יש לנו לפעמים צורך להשוות בין שתי תאריכים. לדוגמה, אם אנו כותבים אפליקציה שמציגה את תאריך הלידה של המשתמש וסופרת כמה ימים נותרו עד ליום המולד, נצטרך להשתמש בשיטת השוואה של תאריכים. בפוסט זה נסביר כיצד לעשות זאת בצורה פשוטה ויעילה בשפת TypeScript.

## איך לעשות

תחילה, נצטרך ליצור שני אובייקטים של תאריך עם התאריכים שברצוננו להשוות. לדוגמה:

```TypeScript
let date1 = new Date(2021, 0, 1); // 1 בינואר 2021 
let date2 = new Date(2021, 5, 15); // 15 ביוני 2021
```

כעת, נשתמש בשיטת השוואה כדי לבדוק את התאריכים. ישנם שלושה אופרטורים שונים שאנו יכולים להשתמש בהם: `>` (גדול מ), `=` (שווה ל) ו- `<` (קטן מ). לפי המקרה שלנו, כדי לבדוק אם `date1` קטן מ-`date2`, נכתוב:

```TypeScript
if (date1 < date2) {
  console.log("date1 קטן מ- date2");
}
```

בפעם הראשונה שישואל למשתמש להשוות בין תאריכים במפתח הפתיחה של האפליקציה, לדוגמה, ניתן להשתמש בקוד הבא:

```TypeScript
function compareDates() {
  let date1 = new Date(prompt("הכנס תאריך ראשון"));
  let date2 = new Date(prompt("הכנס תאריך שני"));
  if (date1 > date2) {
    console.log("date1 גדול מ- date2");
  } else if (date1 === date2) {
    console.log("date1 שווה ל- date2");
  } else {
    console.log("date1 קטן מ- date2");
  }
}

compareDates();
```

כלומר, אם המשתמש יכניס את התאריכים 1/1/2021 ו- 1/1/2020, הודעת הלוג יודפסת "date1 גדול מ- date2" בקונסול. ניתן לשחזר ולבדוק את הפונקציה עם תאריכים שונים