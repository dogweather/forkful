---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:25.932281-07:00
description: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\
  \u0448\u0438\u0431\u043E\u043A (`stderr`) \u043D\u0430\u043F\u0440\u0430\u0432\u043B\
  \u044F\u0435\u0442 \u043A\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043A\u0438\u0435\
  \ \u0441\u043E\u043E\u0431\u0449\u0435\u043D\u0438\u044F \u0432 \u0441\u043F\u0435\
  \u0446\u0438\u0430\u043B\u044C\u043D\u044B\u0439 \u0432\u044B\u0445\u043E\u0434\u043D\
  \u043E\u0439 \u043F\u043E\u0442\u043E\u043A, \u043F\u0440\u0435\u0434\u043D\u0430\
  \u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044B\u0439 \u0434\u043B\u044F \u043E\
  \u0448\u0438\u0431\u043E\u043A, \u043E\u0442\u0434\u0435\u043B\u044C\u043D\u044B\
  \u0439 \u043E\u0442\u2026"
lastmod: '2024-03-13T22:44:45.713019-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A (`stderr`) \u043D\u0430\u043F\u0440\u0430\u0432\u043B\u044F\
  \u0435\u0442 \u043A\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043A\u0438\u0435\
  \ \u0441\u043E\u043E\u0431\u0449\u0435\u043D\u0438\u044F \u0432 \u0441\u043F\u0435\
  \u0446\u0438\u0430\u043B\u044C\u043D\u044B\u0439 \u0432\u044B\u0445\u043E\u0434\u043D\
  \u043E\u0439 \u043F\u043E\u0442\u043E\u043A, \u043F\u0440\u0435\u0434\u043D\u0430\
  \u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044B\u0439 \u0434\u043B\u044F \u043E\
  \u0448\u0438\u0431\u043E\u043A, \u043E\u0442\u0434\u0435\u043B\u044C\u043D\u044B\
  \u0439 \u043E\u0442\u2026"
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
---

{{< edit_this_page >}}

## Что и Почему?
Запись в стандартный поток ошибок (`stderr`) направляет критические сообщения в специальный выходной поток, предназначенный для ошибок, отдельный от основного вывода (`stdout`). Программисты используют его для регистрации ошибок и диагностических сообщений, чтобы они не смешивались с обычными данными программы и их можно было легко отслеживать или перенаправлять.

## Как это сделать:
Swift делает запись в `stderr` простой. Смотрите пример ниже:

```Swift
import Foundation

// Запись в стандартный поток ошибок
func writeToStdErr(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Пример использования
writeToStdErr("Упс! Что-то пошло не так.")

// Вывод при выполнении в консоли может выглядеть так
// (хотя это не будет видно в консоли Xcode):
// Упс! Что-то пошло не так.
```

## Подробнее
В ранние дни программирования различие между `stdout` (стандартный вывод) и `stderr` (стандартный поток ошибок) было жизненно важным для анализа вывода команд и обработки ошибок. Другие языки предлагают похожие конструкции, и в системах, основанных на Unix, эти потоки напрямую связаны с терминалом. Реализация этого в Swift опирается на те же основные принципы, где `stderr` служит небуферизированным потоком, что означает немедленную очистку вывода. Это поведение критически важно для отчетов об ошибках в реальном времени.

Альтернативы включают в себя фреймворки для логирования, которые могут предложить больше функций, таких как уровни логов и форматы сообщений. Собственные стандартные библиотеки Swift достаточно минималистичны; если вам нужна изощренность, вы, вероятно, будете смотреть на сторонние библиотеки или унифицированную систему логирования Apple.

## См. также
Для более глубокого понимания и дополнительного контекста ознакомьтесь с этими ресурсами:

- [Документация об унифицированном логировании Apple](https://developer.apple.com/documentation/os/logging)
- [Справочник стандартной библиотеки Swift для FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
