---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Розбір (parsing) HTML це процес, при якому ми витягуємо корисну інформацію із HTML-документа. Програмісти роблять це для автоматизації вилучення, аналізу, чи модифікації даних з веб-сторінок.

## Як це зробити:
Ми використовуємо бібліотеку Kanna для Swift. Перш ніж почати, інсталюйте Kanna за допомогою Swift Package Manager.
```Swift
dependencies: [
.package(url: "https://github.com/tid-kijyun/Kanna.git", from: "5.2.4")
]
```
Нижче наведено приклад розбору (parsing) HTML:

```Swift
import Kanna

func parseHTML() {
    if let doc = try? HTML(html: "<html><body><h1>Привіт, світе!</h1></body></html>", encoding: .utf8) {
        print(doc.at_css("h1")?.text)
}
}
```
При виконанні цього коду, ви отримаєте результат "Привіт, світе!".

## Поглиблений огляд
▪️ Історичний контекст: Розбір HTML виник, коли стало потрібно автоматизувати процес вилучення даних з веб-сторінок. Із часом, з'явилося багато бібліотек в різних мовах програмування, щоб полегшити цей процес.

▪️ Альтернативи: SwiftSoup - ще одна потужна бібліотека для розбору HTML в Swift. Бібліотеки, як-от jsoup для Java або BeautifulSoup для Python, також можуть бути корисними, залежно від обраної мови програмування.

▪️ Деталі реалізації: Kanna використовує libxml2 і libxslt, які є CPU-ефективними і добре протестованими бібліотеками для розбору XML/HTML і XSLT.

## Дивіться також
[Офіційна документація Kanna](https://github.com/tid-kijyun/Kanna)

[SwiftSoup на GitHub](https://github.com/scinfu/SwiftSoup) 

[jsoup: Java HTML Parser](https://jsoup.org/) 

[Beautiful Soup: We called him Tortoise because he taught us.](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)