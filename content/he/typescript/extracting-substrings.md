---
title:    "TypeScript: חילוץ תת-מחרוזת"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# למה למתקדמים: אחרי קריאת המאמר זה, תלמדו לקודד באמצעות TypeScript בכדי לשלב תת-מחרוזות בתוך הקוד שלכם.

איך לעשות זאת:

מתקדמים משתמשים בשיטות שונות לשלוט ולקרוא מחרוזות בתוך הקוד שלהם. אחת הטכניקות העיקריות היא לחלץ תת-מחרוזות מתוך מחרוזת גדולה יותר. בכדי לעשות זאת באמצעות TypeScript, ניתן להשתמש בפעולת המעבר על מחרוזת ולבחור את התווים הרצויים כדי ליצור תת-מחרוזת חדשה.

```TypeScript
let text: string = "זהו טכנולוגיית TypeScript שמשתמשת בפעולות המעבר";
let subText: string = text.substring(23, 38);
console.log(subText);
```

פלט: "בפעולות המעבר"

## הצצה מעמיקה:

הפעולה המשמשת לחילוץ תת-מחרוזות באמצעות TypeScript היא 'substring' והיא מקבלת שני פרמטרים המייצגים את מיקום התווים הראשונים והאחרונים של התת-מחרוזת שרוצים לחלץ. ניתן להשתמש גם בפעולות נוספות כמו 'slice' ו-'substr' על מנת לחלץ תת-מחרוזות באמצעות TypeScript. אם נרצה לחלץ תת-מחרוזת בהתאמה לתווים ספציפיים, ניתן להשתמש בביטויים רגולריים על מנת להתאים לתווים מסוימים בתוך המחרוזת המקורית.

# ראו גם:

1. [תיעוד רשמי לפעולת המעבר ב-TypeScript](https://www.typescriptlang.org/docs/handbook/string.html#substring)
2. [מדריך מפורט נוסף לחילוץ תת-מחרוזות ב-typeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
3. [מרכז למידה זה ב-Hebrew המתמקד בתת-מחרוזות ב-TypeScript](https://he.code- xd.com/Types-%D7%94%D7%9E%D7%A2%D7%A91%D7%90-%D7%9C%D7%9E%D7%97%D7%A8%D7%A6%D7%95%D7%AA)