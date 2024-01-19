---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט היא פעולה בה מפענחים את המידע שהקובץ מכיל לצורך עיבודו. מתכנתים מבצעים זאת במטרה לגשת למידע קיצוני או לאסוף מידע ממקורות חיצוניים.

## כיצד:

מצערה, בשפת Elm (גרסה הנוכחית) אין את האפשרות לקרוא קבצי טקסט מאופן ישיר. נוסף על כך, Elm נועדה לגשת למידע דרך אתרים אינטראקטיביים בעזרת HTTP.

```Elm
import Http
import Json.Decode as Decode

getRequest = 
    Http.getString "https://example.com/textfile.txt"

task =
    getRequest
    |> Http.send handleResponse
    
handleResponse res =
 case res of 
	Ok txt -> 
	   txt
	Err _ -> 
	   "Error: could not read file" 
```

כאן אנחנו שולחים בקשת GET לקובץ הטקסט ומתמודדים עם התגובה שמוחזרת.

## בחיק העמקים

Elm מאפשרת לנו רק לגשת למידע דרך HTTP משום שהיא שפת תכנות "לקוח קל", מכילה מנגנון חזק של אבטחת מידע. בעבר זו הייתה אמנם אפשרות, אך המפתחים של Elm בחרו להסירה לטובת CET (מנגנון אבטחת מידע מבוססי אמיתות).

קיימות אפשרויות חלופיות במקרה שעבודה מול API באינטרנט לא מספקת את הצרכים שלך, לדוגמה - אתה יכול להשתמש בשפת תכנות שונה (כמו קובץ Node.JS) מבצעת את הקריאה לקובץ ב-danger zone.

## ראה גם:

ישנן מספר מקורות נוספים שיכולים לשמש אתכם:

* תיעוד ה-API של Elm/Http: https://package.elm-lang.org/packages/elm/http/latest/
* דף השאלות והתשובות הנפוצות של Elm: https://elm-lang.org/learn/FAQ
* המדריך לשפת Elm: https://guide.elm-lang.org/