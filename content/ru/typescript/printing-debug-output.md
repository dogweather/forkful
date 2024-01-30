---
title:                "Вывод отладочной информации"
date:                  2024-01-29T00:01:02.798941-07:00
model:                 gpt-4-0125-preview
simple_title:         "Вывод отладочной информации"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Вывод отладочной информации — это ваш фонарик в темном переулке кода; он помогает вам замечать ошибки, позволяя заглянуть в то, что ваш код делает во время выполнения. Программисты делают это потому что, ну, мы люди, и наш код не всегда идеален с первой попытки.

## Как:
Хотите печатать отладочную информацию в TypeScript? Методы консоли - ваш выбор. Смотрите `console.log`, `console.error`, и их друзей в действии:

```TypeScript
// Базовый лог
console.log('Смотри, мам, я отлаживаюсь!');

// Группированные логи
console.group('Данные пользователя');
console.log('Имя: Джон Доу');
console.log('Возраст: 34');
console.groupEnd();

// Таблица
console.table([{ a: 1, b: 'Y' }, { a: 'Z', b: 2 }]);

// Вывод ошибок
console.error('Упс! Что-то пошло не так.');

// Вывод предупреждений
console.warn('Это предупреждение.');

// Отладочный вывод
console.debug('Это отладочное сообщение.');
```

Примеры выходных данных:
```
Смотри, мам, я отлаживаюсь!
Данные пользователя
    Имя: Джон Доу
    Возраст: 34
(index) a  b
0       1  "Y"
1       "Z" 2
Упс! Что-то пошло не так.
Это предупреждение.
Это отладочное сообщение.
```

## Подробнее
В старые добрые времена у нас был `alert()` — он был навязчив и блокировал работу до тех пор, пока с ним не разобрались. Теперь правят бал методы `console`. Они менее навязчивы и обладают суперспособностями: категоризация сообщений, печать таблиц, или стилизация выводов.

Альтернативы? Конечно. Вы могли бы записывать в файл или отправлять сообщения через сеть для удаленного логирования. Для браузера инструменты вроде Chrome DevTools дают вам больше контроля над уровнями и форматами логирования.

С точки зрения реализации, `console` в TypeScript становится JavaScript во время выполнения, и вот где происходит все настоящее действие. Фантазии типов TypeScript здесь не меняют игру — под капотом обычный `console`, в браузере или Node.

## Смотрите также
- [MDN Web Docs о Console](https://developer.mozilla.org/ru/docs/Web/API/Console)
- [Документация Console Node.js](https://nodejs.org/api/console.html)
- [Руководство по TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)