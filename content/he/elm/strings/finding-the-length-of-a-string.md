---
date: 2024-01-20 17:47:49.532090-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E1\u05E4\
  \u05D5\u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9 \u05D1\
  \u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\
  \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05EA\u05E7\u05D9\u05E0\u05D9\u05DD\
  , \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA, \u05D0\u05D5 \u05DC\
  \u05E9\u05DC\u05D5\u05D8 \u05E2\u05DC \u05EA\u05E6\u05D5\u05D2\u05EA \u05D8\u05E7\
  \u05E1\u05D8."
lastmod: '2024-02-25T18:49:37.422303-07:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E1\u05E4\u05D5\
  \u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9 \u05D1\u05D4\
  . \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\
  \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05EA\u05E7\u05D9\u05E0\u05D9\u05DD, \u05DC\
  \u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA, \u05D0\u05D5 \u05DC\u05E9\
  \u05DC\u05D5\u05D8 \u05E2\u05DC \u05EA\u05E6\u05D5\u05D2\u05EA \u05D8\u05E7\u05E1\
  \u05D8."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
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
