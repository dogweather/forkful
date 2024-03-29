---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:10.727439-07:00
description: "\u0420\u043E\u0437\u0431\u0456\u0440 HTML \u0443 Visual Basic \u0434\
  \u043B\u044F \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u043D\u043A\u0456\u0432\
  \ (VBA) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0432\u0438\
  \u043B\u0443\u0447\u0435\u043D\u043D\u044F \u043A\u043E\u043D\u043A\u0440\u0435\u0442\
  \u043D\u043E\u0457 \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457\
  \ \u0437 \u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0430 HTML. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\
  \u0442\u0438\u0437\u0430\u0446\u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:49.005530-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0437\u0431\u0456\u0440 HTML \u0443 Visual Basic \u0434\u043B\
  \u044F \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u043D\u043A\u0456\u0432 (VBA)\
  \ \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0432\u0438\u043B\
  \u0443\u0447\u0435\u043D\u043D\u044F \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\
  \u043E\u0457 \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0437\
  \ \u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0430 HTML. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\u0442\
  \u0438\u0437\u0430\u0446\u0456\u0457\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 HTML"
---

{{< edit_this_page >}}

## Що і чому?

Розбір HTML у Visual Basic для застосунків (VBA) передбачає вилучення конкретної інформації з документа HTML. Програмісти роблять це для автоматизації процесу читання та обробки даних з веб-сторінок, наприклад, для скрапінгу контенту веб-сайту або автоматизації подання форм і отримання даних у таких додатках, як Microsoft Excel або Access, що підтримують VBA.

## Як це зробити:

У VBA можна здійснювати розбір HTML за допомогою `Бібліотеки об’єктів HTML Microsoft`. Додайте посилання на цю бібліотеку у вашому редакторі VBA, перейшовши до Tools > References і відмітивши `Бібліотеку об’єктів HTML Microsoft`. Це надасть вам доступ до класів для навігації та маніпулювання документами HTML.

Ось простий приклад, який показує, як завантажити документ HTML з файлу та вилучити всі посилання (теги якоря):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Завантаження вмісту HTML з файлу
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Ініціалізація документа HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Отримання всіх якірних тегів
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Перебір усіх елементів якоря і виведення атрибуту href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Цей скрипт читає вміст файлу HTML, завантажує його в об'єкт `HTMLDocument`, отримує всі елементи якоря (`<a>` теги) та ітерує їх, друкуючи атрибут `href` кожного у вікно негайних дій.

## Поглиблений аналіз:

Історично розбір HTML у VBA був дещо складним через відсутність прямої підтримки сучасних технологій для веб-скрапінгу та обробки документів. Бібліотека об’єктів HTML Microsoft, незважаючи на свою потужність, є дещо застарілою і може не так гладко обробляти сучасні веб-стандарти, як новітні технології.

Для складних завдань з розбору HTML та веб-скрапінгу часто рекомендують альтернативні інструменти та мови, такі як Python з бібліотеками на кшталт Beautiful Soup або Scrapy. Ці сучасні інструменти пропонують більшу гнучкість, кращу продуктивність та більш відповідають сучасним веб-стандартам. Однак, працюючи в екосистемі Microsoft Office, використання VBA з Бібліотекою об’єктів HTML Microsoft залишається цінним навиком. Це відкриває пряме маніпулювання вмістом HTML способом, який інтегрується безперебійно з такими додатками, як Excel і Access, забезпечуючи прямий метод для виконання завдань, що включають базову обробку документів HTML, без необхідності виходити за межі знайомого середовища VBA.
