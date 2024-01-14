---
title:    "Elixir: בדיקת קיום תיקייה"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"## למה"

בכתיבת קוד בלשון אליקסיר, חשוב לבדוק אם תיקייה קיימת לפני התחלת פעולות עליה. זה יכול למנוע בעיות מיותרות ולהבטיח שהתוכנית תרוץ בצורה אמינה ומדויקת.

"## איך לבדוק אם תיקייה קיימת"

```elixir
def check_directory(directory) do
  if File.exists?(directory) do
    IO.puts "#{directory} קיימת."
  else
    IO.puts "#{directory} לא קיימת."
  end
end

check_directory("documents") 
# documents קיימת.
```

"## חקר מעמיק יותר"

ביצוע בדיקה על קיומה של תיקייה מבוצע באמצעות הפונקציה `File.exists?` שמחזירה ערך בוליאני של `true` אם התיקייה קיימת ו-`false` אם היא לא קיימת. בדיקה זו נעשית באמצעות השתמשות בפונקציה הכיבושית `if`, כך שניתן לתת תנאי לבדיקה האם תיקייה מסוימת קיימת.

"## ראה גם"

- [הגדרת הפונקציה File.exists? באתר הרשמי של אליקסיר](https://hexdocs.pm/elixir/File.html#exists%3F/1)
- [אילן ל מדמה, "כניסה לקובץ ומילוי תיקיות באמצעות אליקסיר"](https://medium.com/@ilanlm/how-to-open-files-and-create-directories-using-elixir-d72e32f1dfea)