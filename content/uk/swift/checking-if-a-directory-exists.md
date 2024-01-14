---
title:    "Swift: Перевірка існування директорії"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливим кроком у програмуванні, оскільки дозволяє переконатися, що необхідний шлях існує перед продовженням виконання програми.

## Як

Для перевірки існування директорії у Swift використовується метод ```FileManager.default.fileExists(atPath: String)```. Нижче наведено приклад коду та результат його виконання:

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents"
if fileManager.fileExists(atPath: path) {
    print("Директорія \(path) існує")
} else {
    print("Директорія \(path) не існує")
}
```

В даному прикладі ми спочатку створюємо об'єкт ```FileManager``` за допомогою методу ```default```. Потім вказуємо шлях до директорії, яку хочемо перевірити. Умовний оператор перевіряє наявність директорії та виводить повідомлення відповідно до результату.

## Глибока деталь

Перевірка існування директорії відбувається за допомогою методу ```fileExists(atPath: String)```, який приймає шлях до директорії в якості вхідного параметра. Цей метод може повернути значення типу ```Bool```, що вказує на наявність або відсутність директорії.

Додаткову перевірку наявності директорії можна виконати за допомогою методу ```fileExists(atPath: String, isDirectory: UnsafeMutablePointer<ObjCBool>)```, який повертає значення типу ```Bool``` та записує вказівник на об'єкт ```ObjCBool```, що вказує на тип шляху (файл або директорія).

## Див. також

- [Офіційна документація Swift з методом fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1407699-fileexists)
- [Стаття на Medium про перевірку існування директорії у Swift](https://medium.com/better-programming/swift-checking-file-or-directory-existence-735afc5cfd14)
- [Стаття на Dev.to про роботу з директоріями у Swift](https://dev.to/gualtierofrigerio/managing-files-and-folders-in-swift-5mmk)