---
title:                "Haskell: המרת מחרוזת לאותיות קטנות"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

כשמתחילים לתכנת ב-Haskell, לפעמים נתקלים במצבים שבהם נרצה להמיר את כל האותיות במחרוזת לאותיות קטנות. מדוע זה נחוץ? הרי רשום הייתי לא תרגיש בנוח לקרוא POTATO במקום potato. למרבה המזל, יש פתרון פשוט ונוח לבעיה הזו שמוגדר ב-Haskell.

## כיצד לעשות זאת

מראה פשוט, כמו הופעה ביצועית כלשהי! במידה ותרצו להמיר את כל האותיות במחרוזת לאותיות קטנות, פשוט נתחיל לכתוב את המחרוזת ונשתמש בפונקציה `map` על ידי כתיבת הפונקציה `toLower` על התוים שמרכיבים את המחרוזת עליהם רוצים להשתמש.

```Haskell
map toLower "POTATO"
```

בפלט הקוד נקבל את המחרוזת "potato" כפי שרצינו!

## צליל לעומק

המרת מחרוזת לאותיות קטנות היא משימה פשוטה וחשובה בתכנות ב-Haskell. במקרים רבים, כשאנו מטפלים במחרוזות, נרצה שהאלפבית שלהם יהיה סטנדרטי ומסודר. כמו כן, המרת מחרוזת לאותיות קטנות היא חשובה לטובת השוואה בין מחרוזות ועבודה עם מחרוזות באופן כללי. בשימוש בפונקצית `map` והפונקציה `toLower`, אנו מקבלים פונקציה יעילה להמרת מחרוזת לאותיות קטנות באופן פשוט ונוח.

## ראה גם

- [Documentation for map function](https://www.haskell.org/hoogle/?hoogle=map)
- [Documentation for toLower function](https://www.haskell.org/hoogle/?hoogle=toLower)