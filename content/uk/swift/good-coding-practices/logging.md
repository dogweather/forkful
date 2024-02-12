---
title:                "Логування"
date:                  2024-01-26T01:08:37.202001-07:00
model:                 gpt-4-1106-preview
simple_title:         "Логування"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/logging.md"
---

{{< edit_this_page >}}

## Що і Чому?
Логування – це процес запису поведінки програми, помилок та іншої важливої інформації до постійного носія, наприклад, файлу чи бази даних. Програмісти роблять це, щоб стежити за працездатністю та продуктивністю своїх додатків, вирішувати проблеми та контролювати, що відбувається за лаштунками в продукційному середовищі.

## Як це зробити:
У Swift ви можете записувати логи до консолі за допомогою операторів print чи гнучкішого API `os.log`, який інтегрується в Уніфіковану Систему Логування на платформах Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Простий оператор print
    print("Початок завантаження")
    
    // Логування події рівня info за допомогою os.log
    os_log(.info, log: logger, "Виконується отримання даних з API.")
    
    do {
        let data = try performNetworkRequest()
        // Логування події рівня debug
        os_log(.debug, log: logger, "Дані отримані: %@", data.description)
    } catch {
        // Логування події рівня error
        os_log(.error, log: logger, "Не вдалося отримати дані: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Імітація мережевого запиту
    return Data()
}
```

Приклад виведення в консоль може виглядати так:

```
Початок завантаження
Виконується отримання даних з API.
Дані отримані: Деякі байти даних...
```

Для помилок виведення може бути:

```
Не вдалося отримати дані: Здається, інтернет-з'єднання відсутнє.
```

## Поглиблено
Логування у Swift набирає нової сили та ефективності з впровадженням Уніфікованої Системи Логування, що була введена в iOS 10 та macOS Sierra. На відміну від оператора `print`, який відправляє все прямо до консолі, ця система базується на діяльності та дозволяє фільтрувати повідомлення логів на основі їхньої важливості та того, чи це збірки для відлагодження чи релізу.

Історичний контекст креслить еволюцію логування в iOS та macOS від примітивних операторів print до всебічних інструментів, які інтегровані з додатками Instruments і Console, забезпечуючи складніші способи аналізу логів.

Існує ряд альтернатив логуванню всередині Swift, наприклад, сторонні бібліотеки, такі як CocoaLumberjack, що пропонує макрорівень над Уніфікованою Системою Логування. Вона забезпечує покращений контроль над форматуванням логів, керуванням файлами та параметрами продуктивності.

В кінці, деталі реалізації; OSLog створена не тільки для ефективності, але і з урахуванням конфіденційності, з можливістю обфускації приватних даних під час логування. Вона розподіляє логи на рівні fault, error, info та debug, кожен з яких пропонує різну глибину для усунення проблем.

## Дивіться також
- [Документація Уніфікованого Логування Apple](https://developer.apple.com/documentation/os/logging)
- [Посібник Ray Wenderlich по логуванню в Swift (os.log)](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Репозиторій CocoaLumberjack на GitHub](https://github.com/CocoaLumberjack/CocoaLumberjack)