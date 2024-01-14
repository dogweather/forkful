---
title:    "Elm: קבלת תאריך נוכחי"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

### למה

תאר לעצמך את הסצנה הבאה: אתה מתחיל לפתח אפליקציה מרתקת ב-Elm, אך נתקלת בצורך להציג למשתמש את התאריך הנוכחי. לפעמים זה יכול להיות כמו להסתובב בתעלומות מבלי לדעת איך לעשות את זה, אבל אין צורך לדאוג יותר! ב-Elm זה פשוט ביותר לקבל את התאריך הנוכחי ובמאמר הזה אני אראה לך כיצד.

### איך לעשות

תחילה ניצור פונקציה פשוטה שקוראת "getDate" ומחזירה את התאריך הנוכחי כמחרוזת.

```Elm
getDate : String
getDate = 
   let
      today = Js.Date.today()
      day = toString(today.getDate())
      month = toString(today.getMonth() + 1)
      year = toString(today.getFullYear())
   in
      day ++ "/" ++ month ++ "/" ++ year
```

זו פונקציה פשוטה שמשתמשת בחבילת JS.Date שיש בכל הדפדפנים ומחזירה את התאריך הנוכחי בפורמט יום/חודש/שנה.

למשל, אם נקרא לפונקציה זו ב-html כך:

```Elm
getDate
```

התוצאה המשומשת תהיה:

```html
03/09/2020
```

אפשר גם להתאים את הפורמט של התאריך לפי צורך, למשל, להציג את התאריך במבנה "יום|חודש|שנה" או כל פורמט אחר.

### מטען עמוק

אם אתה מעוניין להשתמש בפונקציה מעט יותר מתקדמת, אני יפתח עבורך פונקציה שמקבלת את הפורמט והשפה המקומית של המשתמש ומחזירה את התאריך הנוכחי בפורמט מתאים.

```Elm
getLocalizedDate : String -> String -> String
getLocalizedDate format locale =
   let
      options =
         { weekday = Nothing, year = "numeric", month = "long", day = "numeric" }

      result =
         Js.Date.toLocaleDateString(locale, options)

   in
      case format of
         "day-month-year" ->
            String.join "/" <| String.split "," result
         _ ->
            result
```

פונקציה זו מקבלת שני פרמטרים - האחד הוא הפורמט, כמו "יום|חודש|שנה" והשני הוא הש