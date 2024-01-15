---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Fish Shell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# מדוע

הידע לחשב תאריכים בעתיד או בעבר נחשב כמיומנות ייחודית שיכולה להועיל בעבודה מכל סוג ולהקל על חיינו היומיומיים. לדוגמה, ניתן להשתמש בחישובי תאריכים כדי לקבוע מועדים לפגישות או לחצות דרך אירועים חשובים ביומן.

# איך לעשות

דוגמאות קוד ופלט לחישוב תאריכים בעתיד או בעבר בעזרת פקודות שקולטות נתונים:

```fish
# חישוב תאריך בעתיד
set -l date (date -f "%Y-%m-%d" (date --date "tomorrow"))
echo $date
> 2021-11-01

# חישוב תאריך בעבר
set -l date (date -f "%Y-%m-%d" (date --date "10 days ago"))
echo $date
> 2021-10-22
```

# לפנות לעומק

כדי לחשב תאריך בעתיד או בעבר מבוסס על תאריך נתון, ניתן להשתמש בפקודת `date`. ניתן להשתמש בפקודה זו כדי לשנות את תאריך היום הנוכחי, לדעת מה התאריך הנוכחי, או לחשב תאריכים בעתיד או בעבר.

# ראה גם

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [פקודת date ב-Wikipedia](https://en.wikipedia.org/wiki/Date_(Unix))