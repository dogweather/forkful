---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:49.532090-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך מחרוזת זה פשוט לספור כמה תווים יש בה. תכניתנים עושים זאת כדי לוודא שהנתונים תקינים, לבצע בדיקות, או לשלוט על תצוגת טקסט.

## איך לעשות:
ב-Elm, תשתמש בפונקציה `String.length` כדי לקבל את אורך המחרוזת. קבלת אורך של מחרוזת תיראה כך:

```Elm
import Html exposing (text)
import String

main =
  text (String.fromInt (String.length "שלום עולם"))
```

תוצאה:

```
10
```

## עיון מעמיק:
הפונקציה `String.length` של Elm מחזירה את אורך המחרוזת על פי מספר התווים בה. שים לב שב-Elm, שנוצר בשנת 2012, מחרוזת מיוצגת כרשימת תווים וכל תו הוא יחידת Unicode. זה אומר שאורך המחרוזת הוא למעשה מספר התווים ולא המספר הפיזי של הביטים או הבתים. דבר זה הופך את מציאת אורך המחרוזת לאמינה יותר בסביבות רב-תרבותיות.

בעבר, שפות כמו C שימשו את רצף null-terminated כדי לאתר את קצה המחרוזת, אבל בשפות מודרניות כמו Elm זה לא נחוץ. לחלופין, בשפות אחרות יש אסטרטגיות שונות – למשל, פייתון נותנת משקל גם לתווים שאינם לטיניים.

## ראו גם:
- [Elm Documentation for String](https://package.elm-lang.org/packages/elm/core/latest/String#length) - התיעוד הרשמי של Elm לפונקציה `String.length`.
- [Practical Elm for a Busy Developer](https://korban.net/elm/book/) - ספר זה מספק הסברים מעשיים ותרגולים רבים על שפת Elm.
