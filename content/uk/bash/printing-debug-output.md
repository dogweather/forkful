---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Друк тестового виводу - це спосіб, яким програмісти визначають та виправляють помилки у своєму коді. Він дозволяє відстежувати хід виконання коду, а також значення змінних на різних стадіях.

## Як це зробити:

```Bash
#!/bin/bash

DEBUG="true"

if [ "$DEBUG" == "true" ]; then
    echo "Debug mode is ON."
else
    echo "Debug mode is OFF."
fi
```

Коли ви запустите цей скрипт, ви побачите:
```Bash
Debug mode is ON.
```
Відстежуйте значення змінних, використовуючи власні повідомлення про відладку:
```Bash
#!/bin/bash

DEBUG="true"
test_var="Test Variable"

if [ "$DEBUG" == "true" ]; then
    echo "Debug mode is ON. Test_var is $test_var"
else
    echo "Debug mode is OFF."
fi
```
Після запуску скрипта:
```Bash
Debug mode is ON. Test_var is Test Variable
```
## Тематичне погруження:

Створення такого чистого виводу відладки зазвичай неазбідне в Bash, оскільки він вже забезпечує вбудовані засоби відлагодження. Інтерактивний режим (`bash -i`) є одним із них. Проте, форматування власних повідомлень про відлагодження може бути корисним.

Варто відмітити, що в Bash є можливість перенаправляти вивід відлагодження в окремий файл за допомогою команди `exec` і дескрипторів файлів.

У іншіх мовах програмування, таких як Python або JavaScript, для виведення даних використовуються подібні концепції.

## Дивіться також:

1. [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/debugging.html): Подробиці про відлагодження у Bash.
2. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html): Рекомендації щодо стилів у Bash. 
3. [Bash Debugging](https://ryanstutorials.net/bash-scripting-tutorial/bash-debugging.php): Більше інформації про відлагодження в Bash.