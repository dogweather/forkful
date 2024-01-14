---
title:    "Python: Капіталізація рядка"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Стрічки - це один з найбільш базових типів даних у Python. Хочете друкувати їх з великої літери на початку? Ви правильно знайшли цю статтю.

## Як

```Python
my_string = "ukraina"

print(my_string.capitalize())
```

Вихід: "Ukraina"

Як бачите, для того, щоб використати цю функцію, просто потрібно викликати її на своїй стрічці. Це дуже просто і дуже зручно.

## Глибший захоплення

Як ви, напевно, здогадалися, capitalize() - це метод для рядків, що означає, що він доступний для будь-якого рядка. Але ви можете бути здивовані, що він також працює для російських букв. Для прикладу, давайте спробуємо написати "росія" з маленької літери.

```Python
my_string = "росія"

print(my_string.capitalize())
```

Вихід: "Росія"

Це просто вказує на те, що capitalize() враховує регіональні налаштування вашої системи.

## Подивіться також

[Документація з capitalize()](https://docs.python.org/3/library/stdtypes.html#str.capitalize)

[Built-in методи рядків](https://docs.python.org/3/library/stdtypes.html#string-methods)