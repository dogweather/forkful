---
date: 2024-01-20 17:47:49.532090-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elm, \u05EA\
  \u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `String.length`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D0\u05D5\u05E8\u05DA\
  \ \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05E7\u05D1\u05DC\u05EA \u05D0\u05D5\
  \u05E8\u05DA \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05EA\u05D9\u05E8\
  \u05D0\u05D4 \u05DB\u05DA."
lastmod: '2024-03-13T22:44:39.186691-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Elm, \u05EA\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D4 `String.length` \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\
  \u05EA \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

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
