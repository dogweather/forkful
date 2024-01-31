---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:43:53.047829-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і Чому?
Скачування веб-сторінки – це процес збереження її вмісту на ваш комп'ютер. Програмісти роблять це для автоматизації перевірки вмісту, збору даних або тестування веб-ресурсів.

## Як це зробити:
Скачайте сторінку за допомогою `curl` або `wget`:

```Bash
# За допомогою curl
curl http://example.com -o example.html

# За допомогою wget
wget http://example.com
```

Вивід команд виглядатиме приблизно так:

```Bash
# Для curl
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1270  100  1270    0     0   5733      0 --:--:-- --:--:-- --:--:--  5733

# Для wget
--2023-04-14 12:35:47--  http://example.com/
Resolving example.com (example.com)... 93.184.216.34
Connecting to example.com (example.com)|93.184.216.34|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: 648 [text/html]
Saving to: ‘index.html’

index.html          100%[===================>]     648  --.-KB/s    in 0s      

2023-04-14 12:35:47 (94.7 MB/s) - ‘index.html’ saved [648/648]
```

## Поглиблений аналіз
`curl` і `wget` - інструменти командного рядка, з'явились у 90-их. `curl` підтримує більше протоколів, але `wget` - ідеальний для рекурсивного завантаження. Обидва вони дозволяють налаштовувати запити HTTP, на кшталт добавлення заголовків або відправки POST-запитів. Сучасні альтернативи включають GUI-додатки та скриптові мови, але `curl` і `wget` залишаються популярними через свою простоту та гнучкість.

## Додатково
- [cURL офіційна сторінка](https://curl.se/)
- [Wget офіційна документація](https://www.gnu.org/software/wget/manual/wget.html)
- [How to Download Files and Web Pages with Wget](https://www.lifewire.com/uses-of-command-wget-2201085) - стаття про різні варіанти використання `wget`.
