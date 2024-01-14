---
title:    "Elixir: המרת מחרוזת לאותיות קטנות"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## למה

מה הייתה הסיבה ששיקפצת לכתוב פוסט על כיצד להמיר מחרוזת לאותיות קטנות ב-Elixir? כי אנחנו משתמשים הרבה במחרוזות בתכנות, ולכן ישנן מגוון תרגילים ומצבים כאשר יש בידינו מחרוזות שכדאי להמיר אותן לאותיות קטנות. בפוסט הזה נלמד כיצד לעשות את זה בקלות עם השפת תכנות Elixir.

## איך לעשות

בקוד הבא תמצאו דוגמאות של כיצד להמיר מחרוזת לאותיות קטנות ב-Elixir:

```Elixir
# המרת מחרוזת לאותיות קטנות באמצעות הפונקציה String.downcase
String.downcase("Hello World!") #=> "hello world!"

# המרת מחרוזת לאותיות קטנות באמצעות אופרטור ה-shorthand ^=
^"Hello World!" = String.downcase("HELLO WORLD!") #=> true

# בדיקה האם מחרוזת נמצאת רק באותיות קטנות על ידי שימוש בפונקציות Enum.all ו-CString.is_lower
lower_string = "hello world!"
Enum.all?(CString.is_lower, lower_string) #=> true
```

## Deep Dive

הפונקציה String.downcase משמשת כדי להמיר מחרוזת לאותיות קטנות ללא התייחסות לשפת קוד. זאת אומרת שהיא תהיה תואמת לכל שפה, לא רק Elixir. בנוסף, יש לשים לב שהפונקציה מחזירה מחרוזת חדשה ולא משתנה את המחרוזת המקורית.

כרגע, String.downcase תואמת לאותיות הלטיניות בלבד. אם ברצונכם להמיר מחרוזת של אותיות גדולות לאותיות קטנות בכל השפות, אתם יכולים להשתמש בפונקציות כמו Unicode.downcase או Casing.lowercase_string.

## ראו גם

- [String.downcase documentation](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Unicode.downcase documentation](https://hexdocs.pm/elixir/Unicode.html#downcase/1)
- [Casing.lowercase_string documentation](https://github.com/expede/elixir-casing#libraryname)