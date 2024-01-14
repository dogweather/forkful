---
title:                "Elm: קריאת פרמטרי שורת פקודה"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

הזינו אחד או שני משפטים לסביר לקוראים למה הם ישתתפו בקריאת ארגומנטים ממסך הפקודות.

להזין מידע נכון ממסך הפקודות הוא כלי חיוני לכל מתכנת. הכירו את אפשרויות הקוד השונות שמספקת לנו אלם לקריאת ארגומנטים ממסך הפקודות בקלות ובפשטות.

## כיצד לעשות זאת

בשפת אלם ישנן כמה דרכים לקרוא ארגומנטים ממסך הפקודות. הנה כמה דוגמאות של קוד ופלט ממסך הפקודות:

```elm 
module Main exposing (main)
import Platform exposing (worker)
import String
import Task exposing (perform)
import Console exposing (log)

type alias Flags =
	{ file : String
	}

init : Flags -> ((), Cmd Msg)
init flags =
	( (), Cmd.none )

type Msg
	= GotFile String

update : Msg -> () -> ((), Cmd Msg)
update msg _ =
	case msg of
		GotFile file ->
			( (), Cmd.none )

port workerPort : Platform.Flags -> Platform.Flags
port workerPort flags =
	({ flags | tasks = GotFile "Hello World!" })
```

כאן אנו משתמשים בפונקצייות כמו `Platform` ו- `Console` כדי לקרוא את הארגומנטים ממסך הפקודות ולהדפיס אותם על גבי הקונסולה.

## להעמיק

לקריאת ארגומנטים יש לנו גם אפשרות להשתמש בפונקציות כמו `Platform` ו- `Task` כדי לשלוט בארגומנטים ולתפעול פעולות נוספות עליהם. כדאי ללמוד עוד על הפונקציות השונות שמספקות לנו אלם לקריאת ארגומנטים ולהתאים אותן לצרכי הפרויקטים שלנו.

## ראו גם

- Documentation for `Platform` in the Elm website (http://elm-lang.org/docs/).
- Blog post about reading command line arguments in Elm by Richard Feldman (https://dev.to/rtfeldman/how-to-read-command-line-arguments-in-elm-9o).