---
title:                "אינטרפולציה של מחרוזת"
date:                  2024-01-28T21:24:55.866722-07:00
model:                 gpt-4-0125-preview
simple_title:         "אינטרפולציה של מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזות היא השיטה של שילוב ביטויים בתוך מחרוזות קבועות. תוכנים משתמשים בה כדי להכניס ערכים דינמית למחרוזות, מה שהופך את הקוד לקריא ונקי יותר מאשר שרשור מחרוזות מסורתי.

## איך לעשות זאת:
בפייתון בגרסה 3.6 ומעלה, ניתן לאינטרפולט מחרוזות באמצעות f-strings. הנה איך:

```Python
name = 'אליס'
age = 30
greeting = f"שלום, {name}. אתה בן {age} שנים."

print(greeting)
```

פלט:
```
שלום, אליס. אתה בן 30 שנים.
```

ניתן גם להשתמש בביטויים בתוך הסוגריים המסולסלים:

```Python
a = 5
b = 10
info = f"חמישה ועשרה זה {a + b}, לא {2 * (a + b)}."

print(info)
```

פלט:
```
חמישה ועשרה זה 15, לא 30.
```

## טבילה עמוקה
לפני פייתון 3.6, השימוש ב-`.format()` היה הדרך לאינטרפולציה של מחרוזות:

```Python
name = 'בוב'
age = 25
greeting = "שלום, {}. אתה בן {} שנים.".format(name, age)

print(greeting)
```

פייתון מבית ספר ישן (גרסאות < 2.6) השתמשו באופרטור `%` לאינטרפולציה, שהוא פחות אינטואיטיבי ויכול להיות מבולגן עם משתנים מרובים:

```Python
name = 'קרול'
age = 35
greeting = "שלום, %s. אתה בן %d שנים." % (name, age)

print(greeting)
```

מעבר לתחביר נקי יותר, f-strings הם מהירים יותר מפני שהם מוערכים בזמן ריצה ואז מומרים ישירות לפעולת פרמט מחרוזת יעילה. השימוש ב-`.format()` ובאופרטור `%` כרוך ביותר שלבים והוא איטי יותר.

## ראה גם
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) לתיעוד הרשמי על f-strings.
- [Python f-strings](https://realpython.com/python-f-strings/) מאת Real Python למדריך על שימוש ב-f-strings.
- [The .format() Method](https://docs.python.org/3/library/stdtypes.html#str.format) בתיעוד הפייתון להבנת שיטת הפרמוט `.format()` הקודמת של עיצוב מחרוזות.
