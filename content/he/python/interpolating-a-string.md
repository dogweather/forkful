---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:37.214529-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

עיבוד מחרוזות (String interpolation) זה הדרך לשלב משתנים וביטויים בתוך מחרוזת. פיתוחי תוכנה עושים את זה כדי לדינמיות טקסט, להתאים תוצאות, ולקרוא קוד קל יותר.

## איך לעשות:

```Python
# דוגמא עם f-string
name = "יונתן"
age = 30
greeting = f"שלום, קוראים לי {name} ואני בן {age}."
print(greeting)

# דוגמא עם .format()
welcome_message = "ברוך הבא, {}! היום הינו ביום {} בחודש."
print(welcome_message.format("דני", "עשירי"))

# תוצאה
שלום, קוראים לי יונתן ואני בן 30.
ברוך הבא, דני! היום הינו ביום עשירי בחודש.
```

## עיון נוסף:

לפני f-strings היו `%` ו-`.format()`. f-strings, מוצגים ב-Python 3.6+, מאיצים ומקלים על עיבוד מחרוזות. ולמרות ש `%` ו-`.format()` עדיין פעילים, f-strings הם הדרך המועדפת כיום. למעשה, f-strings מבצעים עיבוד מחרוזות בצורה יותר יעילה מבחינת מהירות הריצה.

## ראו גם:

- [תיעוד Python על f-strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [השוואה בין שיטות לעיבוד מחרוזות](https://realpython.com/python-string-formatting/)
- [מדריך ל-`.format()`](https://pyformat.info/)