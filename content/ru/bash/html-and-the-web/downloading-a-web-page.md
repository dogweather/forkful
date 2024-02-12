---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:43.361203-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Скачивание веб-страницы означает извлечение данных из интернета и их сохранение локально. Программисты делают это для веб-скрапинга, анализа в автономном режиме или для создания резервной копии.

## Как это сделать:
Основной инструмент для этой задачи? `curl`. Это мощная командная утилита, которая извлекает данные из сети. Вот самый простой пример использования:

```Bash
curl https://example.com -o webpage.html
```

Эта команда скачивает HTML с `example.com` и записывает его в файл под названием `webpage.html`. Посмотрите на вывод:

```Bash
# Пример вывода
  % Всего    % Получено % Передано  Средняя скорость   Время    Время     Время  Текущее
                                 Скач.  Отпр.   Всего   Потрач.    Ост.  Скорость
100  1256  100  1256    0     0   6458      0 --:--:-- --:--:-- --:--:--  6497
```

Хотите видеть, что вы скачиваете в реальном времени? Уберите `-o`, и скачивание отобразится прямо в вашей консоли:

```Bash
curl https://example.com
```

## Погружение в детали
`curl` существует с 1997 года, завоевав свою нишу в веб-операциях. Почему `curl`, а не скачивание через браузер? Автоматизация и дружелюбие к скриптам. Он неинтерактивен и может быть легко интегрирован в bash-скрипты.

Стоит упомянуть альтернативы: `wget`, другая мощная командная утилита, которая может рекурсивно скачивать веб-страницы. Для серьёзного скрапинга или когда необходим контекст реального браузера, программисты обращаются к инструментам вроде Selenium, Puppeteer или Scrapy.

Изучение работы `curl`: Он поддерживает множество протоколов, от HTTP и HTTPS до FTP, и множество опций (--header, --cookie, --user-agent и т. д.) для настройки запросов. Плюс, обычно он уже предустановлен на системах на базе Unix.

## Смотрите также
- Документация Curl: https://curl.haxx.se/docs/manpage.html
- Руководство Wget: https://www.gnu.org/software/wget/manual/wget.html
- Введение в веб-скрапинг на Python: https://realpython.com/python-web-scraping-practical-introduction/