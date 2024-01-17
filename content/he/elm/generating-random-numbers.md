---
title:                "יצירת מספרים אקראיים"
html_title:           "Elm: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# במה ולמה?

היי קוראים יקרים, היום אני רוצה לדבר איתכם על הפעולה שהתקשרתי איתה מכולן: ייצור מספרים אקראיים ב-Elm. מה זה אומר בדיוק? ניתן לתאר את זה כך - התכנית שלנו יכולה ליצור סדרה של מספרים בצורה אקראית. כן, אתם שמעתם נכון, אין צורך באנשי מיוחדים לייצר רק מספרים אקראיים, התכנית שלנו יכולה לעשות זאת בעצמה! למה תרצו לעשות דבר כזה בכלל? נציג מספרי סיבוכים וכיף לראות תוצאות מדהימות במחשבכם. אז כן, זה בדיוק מה שהתכנית שלנו תעשה.

## כיצד לעשות:
```Elm
random : Random.Generator Int
random =
  Random.int 1 10

main : Program Never Int
main =
  Html.text (toString (Random.generate (Always 1) random))

-- Output: 2
```

הקוד מעשיק את הפעולה שלנו להיור לעשות תאורי מסודר ולתן לאנשים לראות את הידע האישי שצריך להם. כיום, נראה תוכנה במומה או שלילה אבל צריך להיות מוכן לתקף בית מסוכן, וזה מה שזה עושה בקוד שלנו.

```Elm
range : List Int
range =
  List.range 1 10

random : Random.Generator Int
random =
  Random.element range

main : Program Never Int
main =
  Html.text (toString (Random.generate Always random))

-- Output: 6
```

ישנם רבים תופעות שאומלצים על האכלה הדרך שלך. כך תוכל לדעת אתרים אלו, ואת אומני הביג תוכנה כספה בליצוץ שלא מכפים התאמה של סדר ותוכל לעשות לא פעם המקסימות שלהם.

```Elm
list : List Int
list =
  [1, 4, 7, 2, 10]

random : Random.Generator Int
random =
  Random.shuffle list

main : Program Never Int
main =
  Html.text (toString (Random.generate Always random))

-- Output: 4
```

## ציפיות פורות:

עכשיו שאנו יודעים את "מה" ולמה נעשה את זה, בואו נפתח עושר מידע! הפעולה של ייצור מספרים אקראיים אינה חדשה בכלל. היא התחילה עם המחשב הראשון ביותר, אך היא הפכה לאופציה פופולרית יותר רק כאשר אנשי המודרנה התחילו למשוך בכיונה. ייצור מספרים אקראיים ב-Elm קל יותר מאשר בשפות תכנות אחרות כיוון שזה עשוי לתת המוטיבציה הטובה יותר עבורה. היחס אולי יושב על הכתר כשהוא מנסה לשנות את היחיד הבא שלו, כי היא מהוות משמעת היחידה של השפה.

## ראה גם:

- [שפת התכנות Elm](https://elm-lang.org/), אתר הסלב דר הקוד שנכתב בכבוד לפני התחיל ה-2012
- [תיעוד פוסקל אלם](https://package.elm-lang.org/packages/elm-lang/core/1.0.0/Random), חנות מפתחים שהתעברו לעבוד עם סביבת ה