---
title:                "Створення текстового файлу"
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і Чому?
Запис у текстовий файл — це збереження даних у файлі, читабельному для людини. Програмісти це роблять, щоб зберегти результати, конфігурації або передати дані між процесами.

## Як це зробити:
```Fish Shell
echo "Привіт, світ!" > hello.txt   # Створює файл hello.txt і записує текст
cat hello.txt                      # Виведе: Привіт, світ!

echo "Додати цей рядок" >> hello.txt  # Додає текст до існуючого файлу
cat hello.txt                          # Виведе: Привіт, світ!
                                        #         Додати цей рядок
```

## Поглиблений Занурення
Запис у файл здійснювався в UNIX-подібних системах з часів їх створення. Альтернативи Fish Shell включають Bash, Zsh, і Python. Fish використовує `>` і `>>` для запису, де `>` створює/перезаписує, а `>>` додає до файлу. Відмінність Fish — в більш сучасному та простішому синтаксисі.

## Також Рекомендуємо
- Офіційну документацію Fish: https://fishshell.com/docs/current/index.html
- Вступ до Fish Shell: https://tutorial.djangogirls.org/en/optional_fish_shell/
- Статтю про перенаправлення виводу у Fish: https://fishshell.com/docs/current/tutorial.html#tut_redirection
