---
title:                "Elm: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה
רק 1-2 משפטים שמסבירים *למה* מישהו ירצה לקרוא קובץ טקסט.

מעטים מבינים את החשיבות של קריאת קבצי טקסט בתכנות, אך זה כלי חשוב לעיבוד נתונים וכתיבת אפליקציות. באמצעות יכולות ייחודיות של השפה Elm, קריאת קובץ טקסט נעשית בדרך קלה ויעילה.

## כיצד לעשות זאת
בעזרת הדוגמאות שבקוד מתוך " ```Elm...``` " בלוקים שאני מקודד ופלט נתונים שונים, אני אראה לכם כיצד ניתן לקרוא קובץ טקסט בשפה Elm.

```Elm
-- קריאת קובץ טקסט באמצעות String
readTextFile : String -> Task Error String
readTextFile filePath =
    File.read filePath
        |> Task.mapError FileError
        |> taskToResult "Failed to read the file!"

-- פלט נתונים בקובץ Json
outputText : String -> String
outputText str =
    Json.encode str
        |> toString
```

לעולם לא נראה טקסט קווצות כה יזום וברור כמו בשפה Elm!

## מעמקים
כדי להבין את הפעולה של קריאת קובץ טקסט בשפה Elm, עלינו להתרכז בתחום הנתונים. הנתונים נכתבים בקודים המיוחדים והמעודכנים של השפה Elm. ייחודיות זו מאפשרת לנו לבצע פעולות בכותרת חיים דינמית, כדי לתאר מכשולי כתיבה והוספת ביצועים לאופן שבו הפונקציות מטפלות בנתונים.

## ראו גם
הכתבה הזו היא פרק בתוך השפה של המאמר הגדול "Elm Programming 101". ללמוד יותר על קריאת קובץ טקסט בשפה Elm ועוד כלים חשובים, יכולים להיות נספק החינוך הגבוה של הפלטת השפה.