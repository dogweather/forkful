---
aliases:
- /he/elixir/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:31.672633-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Elixir \u05D4\u05D9\u05D0\
  \ \u05E2\u05DC \u05D5\u05D9\u05D3\u05D5\u05D0 \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA\
  \u05D4 \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\
  \u05D1 \u05DE\u05E1\u05D5\u05D9\u05DD \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\
  \u05E7\u05D1\u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\
  \u05D8\u05D9\u05D7 \u05E9\u05D4\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05DE\u05EA\u05D5\u05DB\u05D4, \u05DC\u05DB\u05EA\u05D5\
  \u05D1 \u05D0\u05DC\u05D9\u05D4,\u2026"
lastmod: 2024-02-18 23:08:52.540691
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Elixir \u05D4\u05D9\u05D0 \u05E2\
  \u05DC \u05D5\u05D9\u05D3\u05D5\u05D0 \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA\u05D4\
  \ \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\u05D1\
  \ \u05DE\u05E1\u05D5\u05D9\u05DD \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D1\u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\
  \u05D9\u05D7 \u05E9\u05D4\u05DD \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05E7\
  \u05E8\u05D5\u05D0 \u05DE\u05EA\u05D5\u05DB\u05D4, \u05DC\u05DB\u05EA\u05D5\u05D1\
  \ \u05D0\u05DC\u05D9\u05D4,\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה האם ספרייה קיימת ב-Elixir היא על וידוא נוכחותה של ספרייה בנתיב מסוים במערכת הקבצים. מתכנתים עושים זאת כדי להבטיח שהם יכולים לקרוא מתוכה, לכתוב אליה, או לבצע פעולות עליה בבטחה, מבלי להיתקל בשגיאות עקב העדרה.

## איך לעשות זאת:
הספרייה הסטנדרטית של Elixir מציעה דרך ישירה לבדוק את קיומה של ספרייה דרך המודול `File`. הנה איך אפשר להשתמש בה:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "הספרייה קיימת!"
else
  IO.puts "הספרייה אינה קיימת."
end
```

פלט לדוגמה, בהנחה שהספרייה אינה קיימת:
```
הספרייה אינה קיימת.
```

לאינטרקציות מתקדמות יותר עם מערכת הקבצים, כולל בדיקת קיום ספרייה, ייתכן שתרצו לשקול שימוש בספריות צד שלישי כמו `FileSystem`. למרות שהיכולות הסטנדרטיות של Elixir מספיקות למקרים רבים, `FileSystem` יכול להציע שליטה ומשוב מעודנים יותר ליישומים מורכבים. עם זאת, לצורך הצורך הבסיסי של בדיקה אם ספרייה קיימת, מומלץ בדרך כלל להיצמד למודול ה-`File` המקורי, מאחר שהוא זמין בקלות ולא דורש תלות חיצונית.
