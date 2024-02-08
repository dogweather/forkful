---
title:                "Чтение аргументов командной строки"
aliases:
- ru/fish-shell/reading-command-line-arguments.md
date:                  2024-01-29T00:01:34.580494-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Чтение аргументов командной строки - это получение дополнительных данных, которые вы вводите после имени вашего скрипта, подобно секретным рукопожатиям для настройки поведения скрипта. Программисты делают это, чтобы сделать скрипты гибкими и интерактивными без лишних хлопот.

## Как это сделать:

Допустим, `greet.fish` - это ваш скрипт. Вы хотите, чтобы он принимал имя и выдавал приветствие.

```fish
#!/usr/bin/env fish

# Аргументы хранятся в $argv
# $argv[1] - это первый аргумент, $argv[2] - второй и т.д.

set name $argv[1]
echo "Привет, $name!"
```

Запустите его:

```shell
$ fish greet.fish Мир
Привет, Мир!
```

Теперь с несколькими аргументами:

```fish
#!/usr/bin/env fish

# Проходим через все аргументы
for arg in $argv
    echo "Привет, $arg!"
end
```

Попробуйте:

```shell
$ fish greet.fish Земля Марс Венера
Привет, Земля!
Привет, Марс!
Привет, Венера!
```

Для обработки флагов (например, `-u` для верхнего регистра):

```fish
#!/usr/bin/env fish

# Проверяем наличие аргумента "-u"
set -l uppercase_mode off
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode on
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "on"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

И вызываем:

```shell
$ fish greet.fish -u меркурий венера
МЕРКУРИЙ
ВЕНЕРА
```

## Глубокое Погружение

Fish Shell давно имеет возможности работы с аргументами командной строки, точно так же, как и другие оболочки. Что выделяет Fish, так это его простота по дизайну. Здесь нет необходимости помнить `$1, $2... $n`; это массив `$argv`, знакомая территория, если вы занимаетесь другими языками программирования.

Есть альтернативы, такие как bash, zsh и т.д., но синтаксис скриптов в Fish стремится быть более читаемым и понятным. Вместо традиционных команд `shift` или работы с `$@` для всех аргументов, в Fish есть дружелюбный `$argv` и прекрасные структуры скриптов, такие как циклы `for` и условия `if`, которые меньше о криптических символах и больше о ясных словах.

При реализации важно учитывать, как ваш скрипт будет использоваться. Будут ли нужны значения по умолчанию? Будут ли пользователи знать, что вводить? Убедитесь, что вы обрабатываете случаи, когда пользователи забывают передать аргументы или передают их в неправильном порядке.

## Смотрите также

- Официальная документация Fish по аргументам командной строки: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- Для продвинутого скриптинга и создания собственных функций в Fish: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- Введение в Fish для пользователей с опытом работы в других оболочках: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
