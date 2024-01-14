---
title:    "Gleam: קריאת קובץ טקסט"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# למה

קריאת קובץ טקסט היא תרגיל חשוב ביותר עבור מתכנתי Gleam. בעזרת היכולות העוצמתיות של השפה, קריאת קובץ טקסט היא דרך נוחה לטפל במידע מבנה שונה כגון קבצי CSV או JSON.

# איך לעשות זאת

נקח כלל הכייחנים הפשוט שיש לנו כדי לקרוא קובץ טקסט ולהדפיס את תוכנו למסך. נתחיל על ידי יצירת קובץ טקסט פשוט עם מספר שורות של מילים ונכניס אותו לתוך משתנה בכדי להתמחק בו במהלך הדוגמא. לאחר מכן, נשתמש בפונקצייה המובנית `File.read` כדי לקרוא את הקובץ. לאחר מכן, נשתמש בלולאה על התוכן שנקרא ונדפיס את כל השורות בעזרת הפונקציה `IO.puts`.

```Gleam
let texte = """
Hello World!
I am a text file.
How are you?
"""

File.read("texte.txt")
|> case 
  Ok(text) -> for line in String.lines(text) {
    IO.puts(line)
  } 
  Err(error) -> IO.puts(error)
```

# צלילה עמוקה

כעת נעבור לעומק יותר ונראה מה ניתן לעשות כאשר קוראים קובץ טקסט. נשתמש בפונקציה `String.split` כדי להפריד את התוכן למערך של מחרוזות לפי מפריד מסוים, כגון `"\n"` לשורות חדשות. ניתן גם להשתמש בפונקציה `String.trim` כדי להסיר רווחים ותווים לא רצויים מהתוכן. כמו כן, נוכל לקרוא ולמיין קבצי CSV או קבצי JSON ולהתמודד עם המידע הנמצא בתוכם.

# ראו גם

- [דוגמאות של קריאת קבצי טקסט ב־Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/files/file_read_example.gleam)
- [מדריך לקריאת קבצי CSV ב־Gleam](https://gist.github.com/mindriot101/c002339091c26d587