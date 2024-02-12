---
title:                "Рефакторинг"
date:                  2024-01-29T00:02:23.834848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Рефакторинг"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/refactoring.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Рефакторинг — это процесс перестройки существующего программного кода без изменения его внешнего поведения. Программисты делают это для очистки кодовой базы, улучшения читаемости, поддерживаемости и прокладывания дороги для будущих функций с минимальным техническим долгом.

## Как это сделать:
Давайте начнем с базового примера на Swift, где у нас есть некоторый повторяющийся код:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Имя: \(firstName)")
    print("Фамилия: \(lastName)")
    print("Возраст: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Должность: \(title)")
    print("Компания: \(company)")
}
```

Рефакторинг это включал бы создание структуры `User` для инкапсуляции атрибутов пользователя и добавление метода для печати деталей:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Имя: \(firstName)")
        print("Фамилия: \(lastName)")
        print("Возраст: \(age)")
        print("Должность: \(jobTitle)")
        print("Компания: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Разработчик ПО", company: "Тех Решения")
user.printDetails()
```

### Пример вывода:
```
Имя: John
Фамилия: Doe
Возраст: 30
Должность: Разработчик ПО
Компания: Тех Решения
```

## Подробнее
Рефакторинг имеет корни, которые уходят в ранние дни разработки программного обеспечения, но термин получил популярность в конце 1990-х годов, в частности через фундаментальную книгу Мартина Фаулера "Рефакторинг: Улучшение проекта существующего кода". Книга изложила принцип, согласно которому код должен постоянно очищаться небольшими шагами, а не ожидать отдельной фазы.

Альтернативы ручному рефакторингу включают автоматизированные инструменты и IDE (интегрированные среды разработки), которые могут помочь обнаружить дублированный код, предложить упрощения и автоматически сгенерировать части кода. Xcode для разработки на Swift предлагает различные инструменты рефакторинга, такие как переименование и функциональность извлечения метода, которые могут сократить потенциал человеческой ошибки в процессе.

При реализации рефакторинга важно иметь надежный набор тестов. Тесты служат сеткой безопасности, гарантируя, что вносимые вами изменения не приводят к введению ошибок. Это жизненно важно, поскольку основная цель рефакторинга - изменить внутреннюю структуру без влияния на внешнее поведение.

## Смотрите также
- ["Рефакторинг: Улучшение проекта существующего кода" от Мартина Фаулера](http://martinfowler.com/books/refactoring.html)
- [Документация Swift от Apple](https://swift.org/documentation/)
- [Использование инструментов рефакторинга Xcode](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Руководство по стилю Swift от Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)