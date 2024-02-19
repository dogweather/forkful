---
aliases:
- /he/elixir/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:38.315609-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D1-Elixir \u05D4\
  \u05D9\u05D0 \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D4\u05E4\u05E0\u05D5\u05EA \u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\
  \u05D7\u05D5\u05E0\u05D9\u05DD \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05D4\u05E4\
  \u05DC\u05D8 \u05D4\u05E8\u05D0\u05E9\u05D9 (stdout). \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-stderr \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05EA\u05E8 \u05D5\u05DC\u05D8\u05E4\u05DC \u05D1\u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.542815
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05D1-Elixir \u05D4\u05D9\u05D0\
  \ \u05E9\u05D9\u05D8\u05D4 \u05DC\u05D4\u05E4\u05E0\u05D5\u05EA \u05D4\u05D5\u05D3\
  \u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05D4\u05E4\u05DC\u05D8\
  \ \u05D4\u05E8\u05D0\u05E9\u05D9 (stdout). \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-stderr \u05DB\u05D3\u05D9 \u05DC\
  \u05D0\u05EA\u05E8 \u05D5\u05DC\u05D8\u05E4\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לשגיאה סטנדרטית (stderr) ב-Elixir היא שיטה להפנות הודעות שגיאה ואבחונים בנפרד מהפלט הראשי (stdout). מתכנתים משתמשים ב-stderr כדי לאתר ולטפל בשגיאות מבלי לטעון את הפלט הראשי של התוכנית, דבר המקל על זיהוי וטיפול בבעיות.

## איך לעשות:

ב-Elixir, ניתן להשתמש בפונקציות של מודול `IO` כמו `IO.puts/2` ו-`IO.warn/2` כדי לכתוב הודעות לשגיאה סטנדרטית:

```elixir
# כתיבת הודעה פשוטה ל-stderr
IO.puts(:stderr, "שגיאה: משהו השתבש!")

# שימוש ב-IO.warn, שהוא יותר סמנטי לאזהרות/שגיאות
IO.warn("אזהרה: אתה עומד לחרוג מהמגבלה!")
```

פלט לדוגמה בטרמינל עבור `IO.puts/2`:
```
שגיאה: משהו השתבש!
```

עבור `IO.warn/2`, הפלט יהיה דומה, אך `IO.warn/2` מיועד במיוחד לאזהרות ועשוי לכלול עיצוב או התנהגות נוספים בגרסאות עתידיות של Elixir.

**שימוש בספריות צד שלישי**

למרות שספריית התקנים של Elixir בדרך כלל מספקת מספיק פתרונות לטיפול בפלט שגיאה סטנדרטי, ייתכן שתמצאו ספריות כמו `Logger` שימושיות ליישומים מורכבים יותר או לקביעת רמות ופלטי יומן שונים.

דוגמה לשימוש ב-`Logger` לפלט הודעת שגיאה:

```elixir
require Logger

# קביעת תצורה של Logger לפלט ל-stderr
Logger.configure_backend(:console, device: :stderr)

# כתיבת הודעת שגיאה
Logger.error("שגיאה: נכשל בחיבור לבסיס הנתונים.")
```

הגדרה זו מכוונת את פלט ה-`Logger` באופן ספציפי ל-stderr, דבר שמועיל להפרדת רישום שגיאות מהודעות יומן סטנדרטיות.
