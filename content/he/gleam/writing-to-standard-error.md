---
title:                "Gleam: כתיבה אל שגיאת התקנה"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# למה

עוד רבים מתכנתים לא משתמשים בתהליך הדרגתי של כתיבת שגיאות לשגיאה סטנדרטית. כתיבת שגיאות סטנדרטית היא דרך קלה ויעילה לזהות ולטפל בבעיות בקוד שלנו. במאמר הזה נלמד את מהות הכתיבה לשגיאה סטנדרטית ואת היתרונות שלה. 

# כיצד לעשות זאת

כדי לכתוב לשגיאה סטנדרטית ב-Gleam, נשתמש בפונקציית `io.stderr.write`. לדוגמה, ננסה להדפיס שגיאה סטנדרטית עבור מספר שהוא לא מספר שלם:

```Gleam
iex> import gleam/io
iex> {n, _} = Integer.parse("Not a number")
iex> case n {
iex>   Integer.Integer(i) -> {:ok, i}
iex>   _ -> io.stderr.write("Error: Expected an integer, got " ++ IO.inspect(n))
iex> }
```

כאשר נריץ את הקוד הזה, נקבל את הפלט הבא בקונסול:

```
Error: Expected an integer, got "Not a number"
```

בקלות שניתן לזהות את השגיאה ולהבין מייד מה הולך לא נכון בקוד שלנו. 

# חפירה עמוקה

כשנשתמש בכתיבה לשגיאה סטנדרטית, השגיאות שנוצרות מועברות ל-stderr במקום ל-stdout. זה מאפשר לנו לנקוט בפעולות בקוד שלנו על הפלט שלנו (stdout) כרגיל, ולא לפגום בתהליכי קריאה/כתיבה של קובץ השגיאות. כמו כן, כתיבת שגיאות לשגיאה סטנדרטית גם מאפשרת לנו לקבל פלט מפורט יותר של השגיאות הנוצרות, ולא רק הודעת שגיאה פשוטה. 

# ראה גם

- [Gleam Documentation on Writing to Standard Error](https://gleam.run/documentation/stdlib/io#stderr)
- [Elixir Programming Blog Post on Writing to Standard Error](https://elixir-lang.org/blog/2018/03/22/working-with-standard-error-in-elixir/)