---
title:                "Создание текстового файла"
date:                  2024-01-29T00:05:35.220305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/writing-a-text-file.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Создание текстового файла - это процесс сохранения данных в файле в текстовом формате. Программисты делают это для хранения конфигураций, логов, кода или любых данных, которые необходимо ссылать или сохранять со временем.

## Как это сделать:

```Bash
# Создание нового текстового файла с помощью команды 'echo'
echo "Привет, мир!" > hello.txt

# Добавление текста в существующий файл с использованием оператора '>>'
echo "Еще одна строка текста." >> hello.txt

# Запись нескольких строк с использованием heredoc
cat << EOF > hello_multiline.txt
Привет, это первая строка.
А это вторая строка.
EOF
```

Вывод для `cat hello.txt`:
```
Привет, мир!
Еще одна строка текста.
```

Вывод для `cat hello_multiline.txt`:
```
Привет, это первая строка.
А это вторая строка.
```

## Погружение в детали

Скриптование в оболочке было ключевой частью систем, подобных Unix, начиная с 1970-х годов, причем `sh` (Bourne shell) была оригинальной. Сегодня `bash` (Bourne Again SHell) является широко доступной и используемой оболочкой. Хотя `echo` и перенаправление вывода (`>`, `>>`) являются общими методами записи файлов, альтернативы вроде `printf` предлагают возможности форматирования. Запись файлов в bash-скриптах использует дескрипторы файлов; `1` для `stdout`, и добавление (`>>`) предотвращает перезапись файла за счет использования дескриптора файла `2`.

## Смотрите также

- [Руководство по GNU Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Продвинутое руководство по скриптингу в Bash](https://www.tldp.org/LDP/abs/html/)
- [Учебник по скриптингу в оболочке](https://www.shellscript.sh/)