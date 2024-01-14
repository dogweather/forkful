---
title:    "Haskell: שימוש בביטויים רגולריים"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## מדוע

רק 1-2 משפטים להסביר *למה* מישהו ירצה להשתמש ב regular expressions.

## איך לעשות זאת

לדוגמה, אם אנחנו רוצים למצוא את כל המילים בטקסט שמתחילות באות "ה", נשתמש בפקודה הבאה:

```Haskell
findWordsStartingWithHa :: String -> [String]
findWordsStartingWithHa text = filter (\x -> head x == 'ה') $ words text
```

כאשר נקריא את הפקודה על מחרוזת באורך כלשהו, נקבל רשימה של כל המילים המתחילות באות "ה".

## צלילה מעמוקה

כשמשתמשים ב regular expressions, יש לקחת בחשבון את הביצועים והחוזק של הביטויים. לכן חשוב לא להשתמש בביטויים מיותרים או מורכבים ביותר, ולבחור את הביטוי המתאים ביותר למטרה שלנו.

בנוסף, כדי להבין מלא כיצד משתמשים ב regular expressions בפרקטיקה, מומלץ לעיין בתיעוד ובמדריכים נוספים על הנושא.

## ראה גם

- [תיעוד על ביטויים רגילים והמודול re בספריית Haskell](https://en.wikibooks.org/wiki/Haskell/Regular_expressions)
- [מדריך לregular expressions בספריית Haskell](https://www.codewars.com/kata/a-beginners-guide-to-regular-expressions-haskell/)
- [פרטי מדריכים נוספים בנושא השימוש בregular expressions בHaskell](https://www.google.com/search?q=haskell+regular+expressions+tutorial)