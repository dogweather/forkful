---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת בתכנות היא שיטה לשם צירוף של משתנים או ביטויים בתוך מחרוזת. מתכנתים משתמשים בה כדי ליצור מחרוזות גמישות ונקיות יותר לכל מטרה אפשרית.

## כיצד לכתוב:
```Python
name = "David"
print(f"Shalom, {name}!")  # Output: Shalom, David!
```

```Python
age = 30
print(f"I am {age} years old.")  # Output: I am 30 years old.
```

```Python
num = 5
print(f"The square of {num} is {num**2}.")  # Output: The square of 5 is 25.
```
## צלילה עמוקה 
### הקשר ההיסטורי
האינטרפולציה של מחרוזות היא לא רעיון חדש והייתה חלק משפות תכנות אחרות כמו Perl וRuby לפני שהוכנסה ל-Python 3.6. 

### חלופות 
כדי לצרף משתנים למחרוזת, ניתן להשתמש גם במתודות ישנות יותר של Python כמו הפורמט של מחרוזות או השילוב של מחרוזות:
```Python
name = "David"
print("Shalom, %s!" % name) 
```

```Python
age = 30
print("I am {}. years old.".format(age))  
```

### פרטי היישום
המאפיין העיקרי של תוכנה לאינטרפולציה של מחרוזות הוא תוספת 'f' או 'F' לפני המחרוזת. הסימן קיצון 'f' או 'F' אומר שהמחרוזת היא פורמט תוך שמירה על משתנים כמו שהם.

## ראה גם
1. [מדריך Python - מחרוזות מעוצבות](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)
3. [אינטרפולציה של מחרוזות נכונה ב-Python - Real Python](https://realpython.com/python-f-strings/)