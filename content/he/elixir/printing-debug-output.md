---
title:                "Elixir: הדפסת פלט ניפוי שגיאות"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

מדוע מומלץ להשתמש בהדפסת פלט דיבאג בתכנות ב-Elixir? הדפסת פלט דיבאג היא כלי חשוב לזיהוי ותיקון באגים בקוד שלנו. כאשר אנו מדפיסים פלט דיבאג, אנו יכולים לראות את הערך של משתנים בזמן הריצה ולנתח את הקוד שלנו כדי למצוא את הטעויות שמסתתרות במקום המקור.

## איך לעשות זאת

הנה דוגמאות להדפסת פלט דיבאג ב-Elixir:

```elixir
# הדפסת משתנה
x = 5
IO.puts("The value of x is #{x}")

# הדפסת תנאי
if x == 5 do
  IO.puts("x is equal to 5")
else
  IO.puts("x is not equal to 5")
end
```

תוכלו לראות שלנו משתמשים בפונקציית `IO.puts/1` כדי להדפיס פלט דיבאג. ניתן להשתמש גם בפונקציות נוספות כמו `IO.inspect/2` או `Kernel.inspect/2` להדפסת ערכי משתנים ותוצאות של פונקציות.

## מעמקים

יתר על כן, כאשר אנו משתמשים בהדפסת פלט דיבאג, אנו יכולים גם לצפות בפלט דיבאג במספר סביבות שונות כדי לבדוק את פעולת הקוד שלנו בכל מקום. בנוסף, ניתן לשלב את הדפסת פלט דיבאג עם מנגנוני ניתוח קוד כמו נשק או דבגר למציאת באגים ותקינתם בקוד שלנו.

## ראו גם

למידע נוסף על איך להשתמש בהדפסת פלט דיבאג ב-Elixir, ניתן לעיין במקורות המצורפים להלן:

- [מדריך להדפסת פלט דיבאג](https://elixirschool.com/he/lessons/basics/debugging/)
- [התיעדות ה-FoundationDB](https://github.com/apple/foundationdb/tree/master/fdb-elrang)
- [דוגמה מעשית: מנפצים את הפלט דיבאג](https://medium.com/cookpadengineering