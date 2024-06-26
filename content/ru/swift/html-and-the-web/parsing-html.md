---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:15.422665-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Swift \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0430\u043D\u0430\u043B\u0438\u0437\u0430 HTML;\
  \ \u043D\u0430\u043C \u043D\u0443\u0436\u0435\u043D \u043F\u043E\u043C\u043E\u0449\
  \u043D\u0438\u043A. \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u0435\u043C SwiftSoup, \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F Swift, \u043D\u0430\u043F\u043E\
  \u043C\u0438\u043D\u0430\u044E\u0449\u0443\u044E BeautifulSoup\u2026"
lastmod: '2024-03-13T22:44:45.679488-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Swift \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0433\u043E \u0430\u043D\u0430\u043B\u0438\u0437\u0430 HTML; \u043D\
  \u0430\u043C \u043D\u0443\u0436\u0435\u043D \u043F\u043E\u043C\u043E\u0449\u043D\
  \u0438\u043A."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

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
