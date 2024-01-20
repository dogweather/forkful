---
title:                "Великі літери в рядках"
html_title:           "Bash: Великі літери в рядках"
simple_title:         "Великі літери в рядках"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Капіталізація рядка - це процес перетворення першої букви слова у велику букву. Це корисно, коли програмістам потрібно вивести передбачуваний вигляд тексту, наприклад, для заголовків.

## Як це зробити:

Використовуйте наступний Bash код для капіталізації рядка:

```Bash
$ string="hello, world"
$ echo ${string^}
Hello, world
```

Вихідний результат виводить першу букву рядка, змінену на велику.

## Занурення в деталі:

Перехід від маленьких до великих літер був набагато менш поширеним у минулому. Проте з появою Bash 4 (2009 рік), він став куди простішим завдяки синтаксису `${string^}`.

Альтернативи капіталізації рядка в Bash включають використання `tr`, `awk` або `perl`. Кожен із них має свої вирішальні моменти, але `${string^}` є простим і прямолінійним рішенням специфічно для Bash.

## Дивіться також:

- Bash Guide for Beginners (https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Bash Parameter Expansion (http://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- Transformation of Strings in Bash (https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)