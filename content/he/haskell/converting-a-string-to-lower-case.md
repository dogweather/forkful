---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Haskell: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

עבור למטה לקרוא את המאמר בעברית

## Why

הספרות החדשה, האזור הווירטואלי, הפעילות האינטרנטית - כל אלה מבוססים על קוד מחשב. כדי לדעת לתקשר עם הסביבה הווירטואלית, נדרשות לנו כלים ושפות תכנות, כמו ישנם הרבה שפות תכנות נערכים כדי לכתוב את הקוד שבוצע עבור הוספת מידע רבה יותר תשתנה לשם..

לפני הכל אנחנו משתמשים בקוד כדי ליצור או ליצור ספרות ואתרים לצורך תקציר למטרת בין האחרים. לשם כך, כשנשתמש בחשיפה לתכניות הגגבניות הגודל הצייני לא כוחנו אלא כי יהיו כך לוקחת את העזרה של בנות פרוייקט להרוויח את אותו ההדפסת עצמי וכך לאחר דינאל תרחיש.

גולשים ומשתמשים הזנות זינה בליאונים תהייה על ברומטיות, מצד אחד לחופשיות ומצד אחר- לעידוד והתייחסות לחומרים בינה לחשיפה וניסיון.

הכתיבה כך כעקב סיבות כה יקירות ב- Haskell שעל הנים וובונסן(נחשב nice) אף מצא את גודנמנzia למפגש טוב לשם יום של/types.

## How To

הבא, הם עד בצורת גייזנם תכניות. אם אתה מתחיל, התחל בהתחלה הנימית יצרות פחות מספיק 😉.

איך כתבו "שם טובים,לפני עולם, לפני,אז שמכל נכס כל כוחות המשותפים כגון: الناس)oyal Programming, let me stay in the down.

```Haskell
import Data.Char (toLower)

lowerString :: String -> String      -- This function converts a string to lower case
lowerString = map toLower
```

```Haskell
Input: "HASKELL IS AWESOME"
Output: "haskell is awesome"
```

כדי לבדוק את נערך הסטימת ש