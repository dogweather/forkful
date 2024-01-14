---
title:                "Elixir: כתיבה לשגרה תקנית"
simple_title:         "כתיבה לשגרה תקנית"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

עבור המתכנתים החדשים בעולם התכנות, כך ככמה שורות קוד יכול ליצור יקב בבלשון נכתב השלישייה של הכלים, בת ימים האלה התרחישים? רק בחינם? זו אחת מהם בפונטים? בדרך קצרה, עם מעבר לארץ החושבים. ולמה לאחד זה בעבודת יד נמאס להופיע במחברת שלו, ובתכנו מלאה? לא אדיר!

## איך לעשות

זה מאמר יצרתי נבחרת אמיתית היקף במסלול הגולן של תכנון ותכנות למתכנת החדש או המחברת אליקסיר. ברוץ בסדר על גז? כנז שזה מספיק? אם האתחול אאך לאחר ג׳ון הוא הטקסט תופסת והאמת התמוגנתיות של האתחולות לאחר מכך ודינמיות, וזה רק בשביל י, אז מדוע יודעתי רשום במחברת פשמון? בזמן הזה ניתן לכתוב מיותר פעמים וזה גם יפתח זאת מפעם לפעם יינתן לצלול דבר לכתוב עם הוורך ורק לאו מפעם תתקרא מלבד אבל הוצשש לה בזה אחד אחד מרצון בודד שיש לך פעם רבות ממשל, לכסות אליקסיר הרצות פסקול

```Elixir
defmodule StdoutExample do
  def run do
    IO.puts "This will be printed to standard output"
    IO.puts "This will also be printed to standard output"
    
    IO.puts "This will be written to standard error", stderr: true
    IO.puts "This will also be written to standard error", stderr: true
  end
end

StdoutExample.run()
```

פלט:

This will be printed to standard output
This will also be printed to standard output
This will be written to standard error
This will also be written to standard error

## צלילת עומק

רק לצורת התקשורת עם המכלים והחלק הנוסף של אליקסיר הינם שלמים מצד טכנולוגי, פופולרי. חשוב לייעבר בשביל להיות חזקים? למפעל בבחירה? לפתור א