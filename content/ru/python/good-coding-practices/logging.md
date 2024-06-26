---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:17.822934-07:00
description: "\u041A\u0430\u043A: Python \u043F\u043E\u0441\u0442\u0430\u0432\u043B\
  \u044F\u0435\u0442\u0441\u044F \u0441 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u044B\u043C \u043C\u043E\u0434\u0443\u043B\u0435\u043C \u0434\u043B\u044F\
  \ \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F. \u0412\u043E\
  \u0442 \u0431\u0430\u0437\u043E\u0432\u0430\u044F \u043D\u0430\u0441\u0442\u0440\
  \u043E\u0439\u043A\u0430."
lastmod: '2024-03-13T22:44:44.282363-06:00'
model: gpt-4-0125-preview
summary: "Python \u043F\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442\u0441\
  \u044F \u0441 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u043C \u043C\
  \u043E\u0434\u0443\u043B\u0435\u043C \u0434\u043B\u044F \u043B\u043E\u0433\u0438\
  \u0440\u043E\u0432\u0430\u043D\u0438\u044F."
title: "\u0416\u0443\u0440\u043D\u0430\u043B\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435"
weight: 17
---

## Как:
Python поставляется с встроенным модулем для логирования. Вот базовая настройка:
```Python
import logging

# Базовая конфигурация логирования
logging.basicConfig(level=logging.INFO)

# Сообщения логирования
logging.debug('Это сообщение отладки')
logging.info('Информация о том, что только что сделала ваша программа')
logging.warning('Предупреждающее сообщение')
logging.error('Произошла ошибка')
logging.critical('Программа не может восстановиться!')
```
Когда вы запустите этот код, вы увидите следующий вывод (поскольку уровень по умолчанию - WARNING, сообщения debug и info показаны не будут):
```
WARNING:root:Предупреждающее сообщение
ERROR:root:Произошла ошибка
CRITICAL:root:Программа не может восстановиться!
```
Вы также можете настроить логирование для записи в файл вместо консоли:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Теперь ваши логи будут направлены в файл 'app.log'.

## Глубокое погружение
Логирование существует с первых дней программирования, причем системные логи являются одной из самых старых форм постоянного хранения данных вне фактических файлов с данными. История в сторону, основная концепция логирования по существу осталась неизменной, хотя инструменты эволюционировали.

Модуль `logging` в Python достаточно мощный и гибкий. Он позволяет программистам устанавливать различные уровни логирования (DEBUG, INFO, WARNING, ERROR, CRITICAL), которые могут помочь в категоризации и фильтрации логов. В нем реализована иерархическая система логгеров, что означает, что вы можете иметь отношения родитель-потомок между логгерами и распространять сообщения по цепочке.

Альтернативы включают сторонние библиотеки, такие как Loguru или structlog, которые предлагают расширенные возможности и более простой интерфейс, чем встроенный модуль логирования. Они могут обеспечивать более красивый вывод, лучшую сериализацию структурированных данных и более интуитивные способы работы с конфигурацией логов.

Что касается реализации, при настройке логирования важно сделать это один раз в начале вашего приложения. Рекомендуется настраивать его на уровне модуля, используя `logging.getLogger(__name__)`, чтобы следовать лучшим практикам логирования в Python.

Логирование не должно существенно влиять на производительность приложения в обычных условиях. Однако следует быть осторожным с тем, что логируется: чрезмерно подробное логирование, особенно на уровне DEBUG, может замедлить приложение и быстро заполнить хранилище файлов логов.

## См. также
Для получения дополнительной информации о модуле логирования Python, ознакомьтесь с официальным кулинарным справочником Python по логированию для получения отличных примеров и лучших практик: https://docs.python.org/3/howto/logging-cookbook.html

Для глубокого погружения в структурированное логирование и понимания того, как оно может помочь сделать логи более информативными и легкими для анализа, Loguru хорошо задокументирован: https://loguru.readthedocs.io

Также рассмотрите методологию 12-факторного приложения, в частности раздел о логах для современного взгляда на логирование приложений: https://12factor.net/logs
