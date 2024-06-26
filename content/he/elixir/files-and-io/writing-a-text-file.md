---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:19.759924-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Elixir \u05DE\u05E7\
  \u05DC \u05E2\u05DC \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9\
  \u05DD \u05E2\u05DD \u05DE\u05D5\u05D3\u05D5\u05DC\u05D9\u05DD \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05DD. \u05D4\u05D3\u05E8\u05DA \u05D4\u05E2\u05D9\u05E7\u05E8\u05D9\
  \u05EA \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D4\u05D9\
  \u05D0 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D5\u05EA `File.write/2` \u05D0\u05D5 `File.write!/2`, \u05DB\u05D0\
  \u05E9\u05E8 \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.804235-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u05DE\u05E7\u05DC \u05E2\u05DC \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\
  \u05E7\u05D1\u05E6\u05D9\u05DD \u05E2\u05DD \u05DE\u05D5\u05D3\u05D5\u05DC\u05D9\
  \u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
Elixir מקל על טיפול בקבצים עם מודולים מובנים. הדרך העיקרית לכתוב לקובץ היא באמצעות הפונקציות `File.write/2` או `File.write!/2`, כאשר הראשונה מחזירה טאפל של `:ok` או `:error` והאחרונה מעלה שגיאה במקרה של כשלון.

הנה דוגמה פשוטה:

```elixir
# כתיבה לקובץ, הודעה פשוטה
File.write("hello.txt", "שלום, עולם!")

# כאשר אתה מריץ את הקוד, הוא יוצר את 'hello.txt' עם התוכן "שלום, עולם!"
```

להוספה לקבצים, היית משתמש ב-`File.open/3` עם האופציות `[:write, :append]`, ואז כותב באמצעות `IO.binwrite/2` כדי להוסיף את התוכן:

```elixir
# הוספה לקובץ
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nבואו נוסיף שורה נוספת.")
File.close(file)

# עכשיו 'hello.txt' כולל שורה שנייה "בואו נוסיף שורה נוספת."
```

אם אתה עובד עם נתונים גדולים או זקוק לשליטה יותר בתהליך הכתיבה, ייתכן שתשתמש במודול `Stream` כדי לכתוב נתונים לקובץ באופן עצלני:

```elixir
# כתיבת סט נתונים גדולים בצורה עצלנית
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("מספר: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# זה יוצר את 'numbers.txt', כותב מספרים 0 עד 9, כל אחד בשורה חדשה.
```

לפרויקטים הדורשים טיפול בקבצים מתוחכם יותר, ייתכן שתבדוק ספריות צד שלישי כמו `CSV`, אשר מציעות פונקציונליות מותאמת לניהול קבצי CSV, אך זכור, לרבות מטרות, יכולות הבנויות של Elixir מספיקות ויותר.
