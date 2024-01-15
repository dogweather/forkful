---
title:                "З'єднуючи рядки"
html_title:           "Fish Shell: З'єднуючи рядки"
simple_title:         "З'єднуючи рядки"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Зачем

Багато програмістів часто зустрічаються з необхідністю поєднувати різні рядки тексту в єдиний блок для подальшої обробки і виводу. Найефективніший спосіб зробити це в Fish Shell - це застосувати функцію з'єднання рядків.

# Як

```Fish Shell``` має вбудовану функцію ```string join``` для поєднання рядків. Ця функція працює наступним чином:

```fish
# Код для поєднання рядків
string join <розділювач> <рядок 1> <рядок 2>...
```

Наприклад, якщо у вас є рядок ```"Hello"``` та рядок ```"world"```, і ви хочете поєднати їх разом, додавши між ними кому, ви можете використати наступний код:

```fish
string join , "Hello" "world"
```

Результатом буде рядок ```"Hello, world"```.

# Глибокий погляд

Функція ```string join``` не тільки дозволяє вам з'єднувати прості рядки, але також може бути корисною при з'єднанні значень змінних або результатів виклику інших функцій. Наприклад, ви можете поєднати кілька значень змінних в один рядок за допомогою цієї функції:

```fish
set user "John"
set age 30
string join " is " $user "and is" $age "years old"
```

Результатом буде рядок ```"John is 30 years old"```.

# Дивись також

- [Документація Fish Shell](https://fishshell.com/docs/current/)
- [Покроковий підручник по основах Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Керівництво користувача Fish Shell](https://fishshell.com/docs/current/index.html)