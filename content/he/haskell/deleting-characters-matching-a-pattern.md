---
title:                "Haskell: מחיקת תווים התואמים דפוס"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

בבלוג התכנות שלנו היום נדבר על נושא מעניין - מחיקת תווים התואמים לתבנית בשפת הפקודה הפנימית הסקל. אלו הם פעולות שימושיות לשלוט על מחרוזות ולהוציא מידע מתוכן. אז למה לנו למחוק תווים לפי תבנית? המסלול הברור ביותר הוא להרחיב על כך עם דוגמאות פשוטות כדי שתבינו מדוע זה רלוונטי לכם.

## שיטה

תחילה, ניצור פונקציה פשוטה שתיקח טקסט כקלט ותבדוק אם הוא מכיל תווים התואמים לתבנית מסוימת. ההתאמה מתבצעת באמצעות פונקציית `isPrefixOf` המשווה תת-מחרוזת לתבנית נתונה. אם ההתאמה מתקיימת, אז נשתמש בפונקציית `drop` כדי למחוק את התווים התואמים מהמחרוזת המקורית. לדוגמה:

```Haskell
deleteMatching :: String -> String
deleteMatching str =
  if "Hello" `isPrefixOf` str
  then drop (length "Hello") str
  else str
```

כעת, נרחיב על הפונקציה כדי שתחזיר את המחרוזת המקורית בלי לכלול את התבנית שמתאימה. נשתמש בפונקציית `splitAt` כדי לחלק את המחרוזת לשני חלקים - הראשון תמיד יכיל את התבנית המתאימה, והחלק השני יכיל כל התווים הנותרים. לדוגמה:

```Haskell
deleteMatching :: String -> String
deleteMatching str =
  let (matched, rest) = splitAt (length "Hello) str
  in if "Hello" `isPrefixOf` str
  then deleteMatching rest
  else matched ++ deleteMatching rest
```

בנוסף, אם אנחנו רוצים לשנות את התבנית באמצעות משתנה, ניצור פונקציה המקבלת את התבנית כפרמטר ומשתמשת בו במקום הטקסט הקבוע. הטקסט הזה יכול להיות כל דבר - משתנה, מחרוזת או אפ