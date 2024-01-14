---
title:                "Python: Пошук і заміна тексту"
simple_title:         "Пошук і заміна тексту"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Процес пошуку та заміни тексту є необхідним для будь-якого програміста. Використання такої функціональності дозволяє ефективно змінювати та оновлювати великі обсяги тексту в програмному коді.

## Як це зробити

Python має вбудований метод ```replace()```, який дозволяє замінити одну частину тексту іншою. Наприклад, якщо нам потрібно замінити слово "hello" на "hi" у рядку, ми можемо використовувати наступний код:

```Python
my_string = "Hello World!"
new_string = my_string.replace("Hello", "Hi")
print(new_string)
```

Виконання цього коду дасть нам наступний результат:

```
Hi World!
```

Крім того, метод ```replace()``` можна застосовувати не тільки до рядків, але й до списків, словників та інших типів даних.

## Глибокий пір

Функція ```replace()``` має багато корисних параметрів, які можна використовувати для більш специфічних замін. Наприклад, замість використання методу ```replace()``` для заміни всіх входжень певного слова, ми можемо вкажціть, скільки разів ми хочемо замінити це слово:

```Python
my_string = "Hello Hello Hello!"
new_string = my_string.replace("Hello", "Hi", 2)
print(new_string)
```

Результатом цього коду буде:

```
Hi Hi Hello!
```

Крім того, метод ```replace()``` підтримує використання регулярних виразів для більш складних і специфічних замін.

## Дивись також

- [Документація по методу replace() в Python](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Питання та відповіді про використання методу replace() на Stack Overflow](https://stackoverflow.com/questions/tagged/python+replace)