---
title:                "Перевірка наявності каталогу"
html_title:           "Bash: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перевірка існування директорії є процесом, який дозволяє перевірити чи існує певна директорія на комп'ютері. Це важливо для програмістів, оскільки дозволяє виконувати певні дії з директорією тільки у випадках, коли вона існує.

## Як це зробити:
```
#!/bin/bash

if [ -d /home/user/documents ] 
then
echo "Directory exists"
else
echo "Directory does not exist"
fi
```
В даному прикладі ми перевіряємо чи існує директорія "documents" в домашній папці користувача "user". У разі, якщо директорія існує, на екран буде виведено "Directory exists", інакше буде виведено "Directory does not exist".

## Глибокий погляд:
Існування директорії можна перевірити за допомогою команди "test" та параметру "-d". Ця функціональність була вперше введена у першій версії Shell у 1971 році. Існують також інші способи перевірки існування директорії, такі як використання команди "ls", однак перевірка за допомогою "test" є більш ефективним та швидким способом.

## Дивіться також:
Детальніше про команду "test": https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html#test-invocation
Інші способи перевірки існування директорії: https://www.cyberciti.biz/faq/howto-use-test-to-check-if-file-directory-exists-or-not/