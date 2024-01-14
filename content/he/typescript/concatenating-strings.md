---
title:    "TypeScript: חיבור מחרוזות"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

שלום קוראים!

היום נדבר על חיבור מחרוזות (string concatenation) בשפת TypeScript. למה לעסוק בחיבור מחרוזות בכלל? התשובה פשוטה - חיבור מחרוזות מאפשר לנו לשלב את המידע המאוחד בצורה יעילה ונוחה.

## למה

פונקציות חיבור מחרוזות מאפשרות לנו להתחבר בין מחרוזות שונות כך שניתן ליצור מחרוזות חדשות. בזכות הפונקציות הללו ניתן ליצור מחרוזות חדשות שמכילות את המידע ממחרוזות שונות. בגלל זה, חיבור מחרוזות הוא יישום חשוב בתכנות.

## כיצד לכתוב פונקציה חיבור מחרוזות

תחילה, נגדיר משתנה כדי לאחסן את המחרוזת הראשונה שנרצה לחבר:

```TypeScript
let firstString: string = "שלום לעולם!";
```

אחר כך, נגדיר את המחרוזת השנייה:

```TypeScript
let secondString: string = "אני מתכנת TypeScript";
```

לבסוף, נשתמש באופרטור החיבור (`+`) כדי לחבר את שני המחרוזות יחד:

```TypeScript
let fullString: string = firstString + secondString;
```

הפקודות הללו יצרו את המחרוזת החדשה "שלום לעולם! אני מתכנת TypeScript".

## חקירה עמוקה

אם נבקש לחבר מחרוזות בחוזקה מבלי להשתמש באופרטור החיבור, אנחנו יכולים להשתמש בפונקציות המסופקות על ידי שפת TypeScript. לדוגמה, נוכל להשתמש בפונקציה `string.concat()` כדי לחבר את שתי המחרוזות בצורה יעילה יותר:

```TypeScript
let fullString: string = firstString.concat(secondString);
```

ניתן גם להשתמש בפונקציה `string.join()` כדי לחבר מחרוזות עם מפריד מסוים, כך שניתן להפריד בין המחרוזות השונות. לדוגמה:

```TypeScript
let fullString: string = firstString.concat(" - ", secondString);
```

כ