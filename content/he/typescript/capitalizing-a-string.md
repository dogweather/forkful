---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
קיבוץ של מחרוזת הוא שינוי האותיות הראשונות של כל מילה במחרוזת להיות אותיות רישיות. מתכנתים משתמשים בזה כדי לתת דגש ויזואלי לכותרות ושמות, ולשפר קריאות.

## How to (איך לעשות?)
```TypeScript
function capitalize(sentence: string): string {
  return sentence.replace(/\b(\w)/g, (match, char) => char.toUpperCase());
}

// דוגמת שימוש
const title = 'hello, מתכנתים יקרים!';
console.log(capitalize(title));  // 'Hello, מתכנתים יקרים!'
```

## Deep Dive (צלילה עמוקה)
קיבוץ מחרוזות התחיל כחלק מכללי עריכה באנגלית לפני שהתפשט למחשוב. האלטרנטיבות כוללות שימוש בCSS לקיבוץ במסמכי HTML, או שימוש במתודות מובנות כמו `toLowerCase()` ולאחר מכן `toUpperCase()` לקיבוץ תווים בודדים. הדרך שהצגנו עובדת בJavaScript ו-TypeScript, והיא מנצלת את הRegEx לזיהוי האות הראשונה של כל מילה.

## See Also (ראה גם)
- MDN Web Docs on String methods: https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String
- TypeScript Documentation: https://www.typescriptlang.org/docs/
- RegEx Tester for practice: https://regexr.com/
