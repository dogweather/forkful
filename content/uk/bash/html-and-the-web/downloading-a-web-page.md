---
date: 2024-01-20 17:43:53.047829-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0421\u043A\u0430\u0447\u0430\u0439\u0442\u0435 \u0441\u0442\u043E\u0440\u0456\
  \u043D\u043A\u0443 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E `curl` \u0430\u0431\u043E `wget`."
lastmod: '2024-03-13T22:44:49.574618-06:00'
model: gpt-4-1106-preview
summary: "\u0421\u043A\u0430\u0447\u0430\u0439\u0442\u0435 \u0441\u0442\u043E\u0440\
  \u0456\u043D\u043A\u0443 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u043E\u044E `curl` \u0430\u0431\u043E `wget`."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
