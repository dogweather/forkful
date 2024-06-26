---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:43.543509-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0438\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\u043E\u0441\u043E\u0431\
  \ \u0437\u0430\u0433\u0440\u0443\u0437\u043A\u0438 \u0432\u0435\u0431-\u0441\u0442\
  \u0440\u0430\u043D\u0438\u0446\u044B \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C Fish Shell \u0438 \u043A\u043E\u043C\
  \u0430\u043D\u0434\u044B `curl`."
lastmod: '2024-03-13T22:44:45.837784-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0438 \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0437\
  \u0430\u0433\u0440\u0443\u0437\u043A\u0438 \u0432\u0435\u0431-\u0441\u0442\u0440\
  \u0430\u043D\u0438\u0446\u044B \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435\u043C Fish Shell \u0438 \u043A\u043E\u043C\u0430\
  \u043D\u0434\u044B `curl`."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Вот быстрый и простой способ загрузки веб-страницы с использованием Fish Shell и команды `curl`:

```fish
curl -O http://example.com/
```

Эта команда извлекает содержимое веб-страницы и сохраняет его под тем же именем, что и имя файла на сервере (`index.html` в большинстве случаев).

Теперь, если вы хотите сохранить его под другим именем:

```fish
curl -o my_page.html http://example.com/
```

Хотите видеть, что вы загружаете? Вот как это напечатать в консоли:

```fish
curl http://example.com/
```

Пример вывода может выглядеть так:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Подробнее
В старые времена, извлечение веб-страниц было больше магией командной строки, чем чем-то еще. Инструменты вроде `wget` и `curl` стали основными. `curl`, существующий с '97 года, выдержал испытание временем, доставляя данные с использованием синтаксиса URL.

Почему `curl`, а не `wget`? `curl` - это своего рода армейский нож для передачи данных, работающий с различными протоколами и форматами данных. Хотя оба инструмента могут загружать веб-страницы, `curl` также может загружать данные, поддерживает больше протоколов и часто используется в качестве инструмента для другого программного обеспечения.

Fish Shell сам по себе не загружает веб-страницы; это всего лишь интерфейс. Но объедините его с `curl`, и вы получите мощный, но простой инструмент для загрузки веб-страниц одной строкой.

Некоторые люди могут предложить использовать более современные инструменты вроде `httpie` или автоматизацию на основе браузера с помощью таких инструментов, как Selenium, для более сложных задач, связанных с обработкой страниц, где много JavaScript. Однако для быстрой и простой загрузки `curl` все еще держит оборону.

## Смотрите также
- веб-сайт проекта curl для получения дополнительной информации: [https://curl.se/](https://curl.se/)
- Для более подробного изучения операций HTTP с помощью `curl` см. страницу руководства: `man curl`
- httpie как альтернатива дружественного клиента HTTP: [https://httpie.org/](https://httpie.org/)
- Документация Fish Shell для выполнения других задач, связанных с оболочкой: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
