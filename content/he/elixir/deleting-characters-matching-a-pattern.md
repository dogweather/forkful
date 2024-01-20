---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים התואמים לתבנית היא פעולה שבה אנו מחזירים מחרוזת שבה נמחקו כל התווים התואמים לתבנית מסוימת. מתכנתים משתמשים בכך כדי לנקות מידע או כדי לחלץ מידע מסויים ממחרוזות.

## איך לעשות:

נבדוק איך נעשה זאת באליקסיר:

```Elixir
defmodule Clean do
  def remove_chars(str, pattern) do
    String.replace(str, pattern, "")
  end
end
```

אם תרצו להסיר את כל האותיות 'a' מתוך מחרוזת:

```Elixir
IO.puts(Clean.remove_chars("Banana", "a"))
```

הוצאה:

```Elixir
"Bnn"
```

## מעומק חפיפה

התפקוד הזה בא לידי ביטוי בשפות תכנות רבות, ומאפיין מרכזי של עיבוד מחרוזות. אלטרנטיבות לפונקצייה זו כוללות את השימוש בפונקציות regex או אולי לבנות את הפונקציה שלך משולש לבנות. באופן כללי, זהו חלק מהפעולות הבסיסיות על מחרוזות שמתכנת צריך לדעת על מנת להתמודד עם מקרים אמיתיים.

## ראה גם

המשאבים הבאים מספקים פרטים נוספים והסברים בנושאים קשורים:

1. [Elixir String Documentation](https://hexdocs.pm/elixir/String.html) - ההסבר הרשמי והמקיף ביותר לעבודה עם מחרוזות באליקסיר.
2. [Elixir School String Functions](https://elixirschool.com/en/lessons/basics/strings) - מקור נוסף שמתמקד במיוחד בפונקציות מחרוזות.
3. [Hexdocs Regular Expressions guide](https://hexdocs.pm/elixir/1.12/Regex.html) - מעבר על Regular Expressions באליקסיר, כולל איך להשתמש בהם כדי למצוא תבניות במחרוזות.