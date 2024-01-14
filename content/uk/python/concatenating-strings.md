---
title:                "Python: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/concatenating-strings.md"
---

{{< edit_this_page >}}

### Чому
У зв’язку з тим, що рядкове з’єднання є важливим принципом програмування, це навичка, яку ви повинні вивчити, щоб стати успішним програмістом Python.

### Як це зробити
```Python
# Ви можете з’єднати рядки, використовуючи оператор "+"
string_1 = "Привіт"
string_2 = "світ!"
result = string_1 + " " + string_2
print(result)
```

Вихід: `Привіт світ!`

```Python
# Ви також можете використовувати метод ".join ()"
list_of_strings = ['Навіть', 'цікавіші', 'речення']
result = " ".join(list_of_strings)
print(result)
```

Вихід: `Навіть цікавіші речення`

### Глибше
При з’єднанні рядків, важливо пам’ятати про те, що рядки пов’язані зі змінними будуть перетворені в рядок, незалежно від типу даних цих змінних. Також, при використанні ".join ()" методу, список аргументів повинен бути типом рядок.

### Дивись також
- [Python String Concatenation](https://www.w3schools.com/python/gloss_python_string_concatenation.asp)
- [Python String manipulation](https://realpython.com/python-string-split-concatenate-join/)