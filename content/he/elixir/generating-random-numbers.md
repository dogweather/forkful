---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

הפקת מספרים אקראיים היא תהליך שבו מחוללים מספרים באופן חסר תכלית. מתכנתים משתמשים בזה לשם צורכים רבים, כולל בדיקות, משחקים, אבטחת מידע, ועוד.


## איך לעשות:

אליקסיר מספקת ספריה אינטגרלית להפקת מספרים אקראיים. הנה כיצד להשתמש בה:

```Elixir
:rand.uniform()
# => 0.9862288928553453
```
הפונקציה `uniform/0` מחזירה מספר אקראי בין 0 ל-1.
אפשר לקבוע טווח קצה על ידי ספקת מספר כארגומנט.

```Elixir
:rand.uniform(10)
# => 7
```
זה מחזיר מספר אקראי בין 1 ל-10.

## צלילה עמוקה:

אחת האלטרנטיבות הראשונות להפקת מספרים אקראיים התבצעה על ידי הפעלת הפונקציה `random:uniform/1` שנמצאת ב- Erland. בעקבות OTP 19, ארלנג התחילה להשתמש במודול ה- ':rand'. שימוש ב- ':rand' או ב-':random' זהים, אבל ':rand' הוא העדיף מאחר והוא גם עדכני ובטוח יותר. יש לשים לב שכל מופע של ':random' טעון לאתחול מפורש בעת התחלת העבודה.

## ראו גם:

1. מסמך על `:rand` ב- Erlang/OTP : http://erlang.org/doc/man/rand.html
2. מודול עזר נוסף בספריה התקנית של אליקסיר : https://hexdocs.pm/elixir/1.12/Kernel.html#module-random-number-generation
3. מדריך על הפקת מספרים אקראיים: https://learning.oreilly.com/library/view/elixir-in-action/9781617295057/OEBPS/Text/09.xhtml