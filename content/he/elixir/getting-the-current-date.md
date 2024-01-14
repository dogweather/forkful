---
title:                "Elixir: לשיגור תאריך נוכחי"
simple_title:         "לשיגור תאריך נוכחי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, תמיד חשוב להיות מכירים בשימוש בתאריך הנוכחי. התאריך הנוכחי מועיל במגוון רחב של מטרות, כגון התייחסות לזמן במשחקים או ביישומי תוכנה, הדגמת תאריך בצורה חכמה למשתמשים או פשוט לשימוש אישי.

## איך לעשות זאת

ראשית, נטען את המודול `Calendar` של Elixir:

```
Elixir
iex> require Calendar
```

כעת, נשתמש בפונקציה `local_time` כדי לקבל את התאריך הנוכחי לפי האיזור הזמן המקומי שלנו:

```
Elixir
iex> Calendar.local_time
```

אם נרצה לקבל את התאריך באיזור זמן אחר, נוכל להשתמש בפונקציה `convert_time`:

```
Elixir
iex> Calendar.convert_time(Calendar.local_time, "UTC")
```

ניתן גם לבחור להציג את התאריך בפורמט מסוים, כך:

```
Elixir
iex> Calendar.local_time
~U[2019-05-08 16:20:45]
```

## לעמוד על התהליך

כאשר אנו משתמשים בפונקציות כמו `local_time` ו-`convert_time`, אנו בעצם מפעילים תהליך מתוחכם של חישוב תאריך ושעה בהתאם לאיזור הזמן ולפורמט שציינו. כל החישובים מתבצעים בתוך המודול `Calendar` של Elixir, המעניק לנו גם אפשרויות נוספות כמו חישוב התאריך הנוכחי בלי פירוט לשניות או מיליוניות שנייה, או תמיכה בערכי time zones נוספים.

## ראה גם

- [Elixir Calendar documentation](https://hexdocs.pm/elixir/Calendar.html)
- [UTC vs. GMT: What's the Difference?](https://www.timeanddate.com/time/zones/utc-gmt-difference.html)
- [Unix Time: What Is It and Why Should I Care?](https://www.unixtimestamp.com/)