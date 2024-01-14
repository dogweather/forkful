---
title:    "Elixir: מציאת האורך של מחרוזת"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה
תוכלו להשתמש בפונקציית `String.length` על מנת למצוא את אורך המחרוזת שלכם ולנהל אותה באופן יעיל ומנקודת מבט של זמן.

## איך לעשות זאת
```Elixir
# גישה פשוטה
string = "שלום עולם"
length = String.length(string)

IO.puts(length)
# output: 11
```

```Elixir
# גישה מתקדמת יותר עם pattern matching
string = "אני אוהב אליקסיר"
length = String.length(string)

# הדפסת רק אם האורך שווה ל-12
case length do
  12 -> IO.puts("האורך של המחרוזת זה 12!")
  _ -> IO.puts("האורך של המחרוזת לא 12")
end

# output: האורך של המחרוזת זה 12!
```

## חפירה עמוקה
במאמר זה, למדנו איך למצוא את אורך המחרוזת באמצעות פונקציית `String.length` וגם להשתמש באותו עקרון עבור פונקציות אחרות של מחרוזות. נתחיל להשתמש בצורה נכונה של קוד כאשר מתייחסים למחרוזות שונות עם הרבה שפות כדי ליצור קוד יעיל ונקי.

## ראו גם
- [קבוצת המשתמשים הרשמית של Elixir בישראל](https://www.facebook.com/groups/Elixir.Israel/)
- [אתר רשמי של Elixir](https://elixir-lang.org/)
- [מדריך למתחילים ב-Elixir](https://elixir-lang.org/getting-started/introduction.html)