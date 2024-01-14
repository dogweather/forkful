---
title:                "Bash: Виведення відлагодженого виводу"
simple_title:         "Виведення відлагодженого виводу"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Чому

Написання програм іноді може бути викликом, і коли у вас є проблеми з кодом, візуальне відслідковування процесу виконання може значно полегшити життя. Один зі способів, яким можна це зробити, це за допомогою виведення діагностичної інформації.

# Як це зробити

```Bash
#!/bin/bash
echo "Це повідомлення дебагу" # виводить повідомлення дебагу на екран
```

Цей код буде виводити повідомлення "Це повідомлення дебагу" під час виконання скрипту.

# Глибше

Виводячи дебаг-інформацію, ви можете бачити, як ваш код виконується в реальному часі. Це може допомогти виявити помилки та зрозуміти, що саме веде себе неправильно.

# Дивіться також

- [Стаття про виведення діагностичної інформації у Bash](https://linux.die.net/man/1/echo)
- [Документація з використання команди echo](https://www.gnu.org/software/bash/manual/html_node/Echo-Collapse.html#Echo-Collapse)
- [10 команд у Bash, яким варто навчитися](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)