---
title:    "Elixir: שרשור מחרוזות"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

פעם בשעה הבאה שאתם מצפים לצרוך מחרוזת, אל תשכחו לשכוח את הזכרון הקסום שלנו ללמוד את הפונקציה `String.concat/2` ב-Elixir. תוכלו להשתמש ב־`String.concat/2` כדי לחבר מספר מחרוזות ליחד בקלות ובמהירות.

## איך לעשות זאת

כדי להשתמש ב־`String.concat/2`, עליכם לספק שתי ארגומנטים: מערך של מחרוזות ואופציונלית נוספת עם ערך ברירת מחדל של אופציונלית ריקה.

```elixir
my_strings = ["היי", "שלום", "אהלן"]
String.concat(my_strings)  # החזרה: "היישלוםאהלן"
```

בנוסף, ניתן להשתמש ב־`String.concat/1` כדי לחבר מערך של איברים אקסוטיים מסוג מחרוזת ולהמיר אותם למחרוזת:

```elixir
numbers = [1, 2, 3]
String.concat(numbers)  # החזרה: "123"
```

## חקירה עמוקה

איך `String.concat/2` עובדת בדיוק? ברקופדה של אריכזה (String concatenation), איברים מסוג מחרוזת מוסיפים זה לזה כדי לקבל מחרוזת חדשה. ב-Elixir, פעולת האיבר הכי בסיסית ברקופדה היא פשוט להוסיף את המחרוזות הנתונות יחד ללא פסיק ביניהן.

בכמה שהפעם אתם משתמשים ב־`String.concat/2`, אנחנו מקווים שאתם תרגישו מתוך הטבע שתיפרקו לוביים מחרוזת ידנית, ותתחילו להשתמש בפונקציות כמו `String.split/2` ו־`Enum.join/2` כדי לחזור על אותה פעולה בקוד מספר פעמים עם קוד נמוך יותר וקל יותר לתחזוקה.

## ראו גם

- [תיעוד רשמי על `String.concat/2`](https://hexdocs.pm/elixir/String.html#concat/2)
- [פורום הדיון על ביצוע מחרוזות בקוד הודעות של Elixir](https://elixirforum.com/t/how-to-concatenate-strings-in-elix