---
title:                "Проверка существования директории"
date:                  2024-01-28T23:55:36.371790-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Проверка наличия директории заключается в верификации присутствия папки на вашем носителе перед выполнением с ней каких-либо действий. Программисты делают это, чтобы избежать ошибок, например, попытки создать уже существующую директорию или доступа к несуществующей.

## Как это делать:
Работа с директориями на Arduino часто включает использование библиотеки SD для хранения данных на карте SD. Сначала убедитесь, что ваш Arduino правильно подключен к модулю карты SD. Затем вы используете функцию `SD.exists()` для проверки существования директории. Вот короткий пример:
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // ожидание подключения к последовательному порту. Необходимо только для нативного USB-порта
  }

  if (!SD.begin(4)) { // Обязательно используйте правильный пин выбора чипа
    Serial.println("Initialization failed!");
    return;
  }

  if (SD.exists("/example")) {
    Serial.println("/example директория существует.");
  } else {
    Serial.println("/example директория не существует.");
  }
}

void loop() {
  // Здесь нечего делать
}
```
Пример вывода, когда директория существует:
```
/example директория существует.
```
И когда ее нет:
```
/example директория не существует.
```
Не забудьте заменить `/example` на фактический путь, который вы хотите проверить.

## Подробный анализ
Еще недавно проверка существования директории не всегда была прямолинейной. Системы имели различные команды. В случае с Arduino библиотека SD сделала это последовательно, заимствуя концепции из стандартных практик программирования.

Что касается альтернатив, если вы работаете с не-SD хранилищем или нуждаетесь в большем контроле, другие библиотеки, такие как SdFat, предоставляют аналогичные функции с дополнительными возможностями. Некоторые продвинутые реализации могут взаимодействовать с файловыми системами более непосредственно, но для большинства пользователей SD.exists() достаточно.

Проверка директории включает в себя запрос библиотекой у файловой системы информации о специальной файловой записи, которая представляет директорию. Если она есть, отлично. Если нет, вы получите false. Библиотека SD управляет низкоуровневой коммуникацией между вашим Arduino и файловой системой носителя, абстрагируя грубые детали — так что вы получаете необходимую информацию без хлопот.

## См. также
- Справочник по библиотеке SD Arduino: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- Библиотека SdFat для более надежного взаимодействия с картой SD: [https://github.com/greiman/SdFat](https://github.com/greiman/SdFat)