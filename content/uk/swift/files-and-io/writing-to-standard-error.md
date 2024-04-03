---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:06.918914-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Swift \u0437\u0430\u043F\u0438\u0441 \u0443 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u0438\u0439 \u043F\u043E\u0442\u0456\u043A \u043F\u043E\
  \u043C\u0438\u043B\u043E\u043A \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\
  \u043E\u043D\u0430\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\
  \u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `FileHandle` \u0434\u043B\u044F\
  \ \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0434\u043E\u0441\u0442\u0443\u043F\
  \u0443 \u0434\u043E stderr. \u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\u0438\
  \u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.947739-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Swift \u0437\u0430\u043F\u0438\u0441 \u0443 \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u0438\u0439 \u043F\u043E\u0442\u0456\u043A \u043F\
  \u043E\u043C\u0438\u043B\u043E\u043A \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\
  \u043A\u043E\u043D\u0430\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `FileHandle` \u0434\u043B\
  \u044F \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0434\u043E\u0441\u0442\u0443\
  \u043F\u0443 \u0434\u043E stderr."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

## Як це зробити:
У Swift запис у стандартний потік помилок можна виконати за допомогою класу `FileHandle` для прямого доступу до stderr. Ось простий приклад:

```swift
import Foundation

// Визначте повідомлення
let errorMessage = "Трапилась помилка.\n"

// Конвертуйте повідомлення у дані
if let data = errorMessage.data(using: .utf8) {
    // Запишіть повідомлення про помилку в stderr
    FileHandle.standardError.write(data)
}
```

Вивід в stderr (зазвичай переглядається в консолі або терміналі):
```
Трапилась помилка.
```

Для більш складного логування або при роботі з зовнішніми бібліотеками можна розглянути використання сторонньої бібліотеки, як-от **SwiftLog**. Хоча **SwiftLog** не записує прямо у stderr з коробки, ви можете реалізувати власний бекенд логування для досягнення цього. Ось спрощений приклад визначення власного обробника логу, який записує в stderr:

Спочатку додайте **SwiftLog** до залежностей вашого проекту в `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Потім реалізуйте власний обробник логу, який записує в stderr:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// Використання
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("Це повідомлення про помилку")
```

Вивід в stderr:
```
Це повідомлення про помилку
```

Цей власний обробник дозволяє направляти ваші повідомлення про помилки SwiftLog безпосередньо у стандартний потік помилок, інтегруючись безперервно з іншими повідомленнями логу, які може генерувати ваш додаток.
