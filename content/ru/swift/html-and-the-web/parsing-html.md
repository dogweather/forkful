---
title:                "Разбор HTML"
aliases: - /ru/swift/parsing-html.md
date:                  2024-01-29T00:00:15.422665-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Анализ HTML представляет собой процесс просеивания кодовой базы веб-сайта для поиска полезных элементов — текстов, ссылок, изображений и т. д. Программисты делают это для извлечения данных, автоматизации веб-взаимодействий или импорта контента в свои приложения.

## Как это сделать:

В Swift нет встроенного анализа HTML; нам нужен помощник. Давайте используем SwiftSoup, библиотеку для Swift, напоминающую BeautifulSoup для Python. Сначала добавьте SwiftSoup в ваш проект с помощью Swift Package Manager.

Вот как это сделать:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Первый анализ</title></head>"
                + "<body><p>HTML анализирован в документ.</p></body></html>"
    let doc = try SwiftSoup.parse(html)
    let title = try doc.title()
    let bodyText = try doc.body()?.text()
    
    print(title) // Вывод: Первый анализ
    print(bodyText) // Вывод: HTML анализирован в документ.
} catch Exception.Error(let type, let message) {
    print("Произошла ошибка типа: \(type): \(message)")
} catch {
    print("Произошла неизвестная ошибка")
}
```

## Глубокое погружение

HTML или язык гипертекстовой разметки стал основой веба с тех пор, как Тим Бернерс-Ли представил его (и веб) в 1991 году. По мере развития веба развивался и HTML, усложняя анализ.

Вот почему SwiftSoup выделяется:
- **Дружелюбный к пользователю**: Его API повторяет JQuery, что делает его интуитивно понятным для тех, кто знаком с веб-разработкой.
- **Надежность**: Хорошо справляется с реальными особенностями HTML.
- **Производительность**: Swift быстрый, что важно для больших задач анализа.

Альтернативы? Конечно!
- **WebKit**: Используйте это для более тяжелых задач, таких как рендеринг веб-страниц или выполнение JavaScript.
- **libxml2**: Жесткий путь через C, но будьте готовы к вызову.
- **Regex**: Просто нет. Это не анализатор. Не пытайтесь "анализировать" HTML с помощью regex. Серьезно.

Однако помните, что анализатор вроде SwiftSoup не просто читает страницу как есть; он не воспринимает контент, динамически загружаемый с помощью JavaScript. Для этого лучше обратиться к решениям, связанным с WebKit или безголовыми режимами браузеров.

## Смотрите также

- SwiftSoup на GitHub: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift Package Manager: [https://swift.org/package-manager/](https://swift.org/package-manager/)
- Документация WebKit: [https://developer.apple.com/documentation/webkit](https://developer.apple.com/documentation/webkit)
- Работа с динамическим контентом: [Selenium WebDriver](https://www.selenium.dev/documentation/en/) (не специфично для Swift, но актуально для автоматизированных взаимодействий с динамическими веб-страницами)
