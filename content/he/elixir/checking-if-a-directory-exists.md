---
title:                "Elixir: לבדיקה אם תיקייה קיימת"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

בכדי לבנות יישומים מבוססי ווב או סקרייפטים מבוססי משתמש בקבצים, חשוב לדעת את המיקום שלהם במערכת הקבצים. כדי לוודא שאנו מחפשים קבצים על ידי הפעלת כלכלת מבנה של הנתיבים המסומנים מרחבת הזמן של "משתמשים", יש לבדוק אם התיקייה oקיימת לפני שאנו מנסים לגשת אליה ולעבור על הקבצים המסוממים בתוכה. בכך אנו מונעים שגילוף תיקיות נוצרות לבד ומאפשרים לנו לוודא את הפעלות ההנאה שלנו במערכת הקבצים באופן יעיל יותר.

## איך לבצע זאת

```Elixir
defmodule FileCheck do
  def does_exist?(directory) do
    File.dir?(directory)
  end
end

FileCheck.does_exist?("/Users/Desktop/Example")
# output: true
FileCheck.does_exist?("/Users/Desktop/Nonexistent")
# output: false
```

## מגע עמוק

כאשר אנו משתמשים בפונקציות על גבי המערכת הקבצים באליקסיר, הן מחזירות ערך בוליאני שמציין האם הקובץ או התיקייה קיימת או לא. לדוגמא, הפונקציה File.dir? מקבלת כפרמטר קבצים או תיקיות ובודקת אם הם קיימים. אם הקובץ או התיקייה קיימת, היא מחזירה ערך true, אחרת היא מחזירה false. בכדי לוודא את תנאי הקיום של קבצים או תיקיות מסוימות, ניתן להשתמש בפונקציות נוספות כגון File.regular?, המקבלת כפרמטר קבצים ובודקת אם הם קיימים והאם הם קבצים נורמליים.

## ראו גם

- [פוקנציות מערכת הקבצים באליקסיר](https://hexdocs.pm/elixir/File.html)
- [קריאת קבצים ותיקיות ממערכת הקבצים באליקסיר](https