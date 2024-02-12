---
title:                "Анализ HTML"
aliases:
- /ru/vba/parsing-html.md
date:                  2024-02-01T21:57:12.623272-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/vba/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Разбор (парсинг) HTML с использованием Visual Basic for Applications (VBA) включает в себя извлечение определенной информации из HTML-документа. Программисты делают это для автоматизации процесса чтения и обработки данных с веб-страниц, таких как собирание содержимого сайта или автоматизация отправки форм и получения данных в таких приложениях, как Microsoft Excel или Access, которые поддерживают VBA.

## Как это сделать:

В VBA вы можете разбирать HTML, используя `Библиотеку объектов HTML Microsoft`. Добавьте ссылку на эту библиотеку в вашем редакторе VBA, перейдя в Инструменты > Ссылки и отметив `Библиотеку объектов HTML Microsoft`. Это дает вам доступ к классам для навигации и манипулирования HTML-документами.

Вот простой пример, который показывает, как загрузить HTML-документ из файла и извлечь все ссылки (теги якоря):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Загрузка HTML-содержимого из файла
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Инициализация HTML-документа
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Получение всех тегов якоря
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Перебор всех элементов якоря и печать атрибута href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Этот скрипт читает содержимое HTML-файла, загружает его в объект `HTMLDocument`, извлекает все элементы якоря (`<a>` теги) и затем перебирает их, печатая атрибут `href` каждого в окно немедленного выполнения.

## Подробнее:

Исторически разбор HTML в VBA был немного громоздким из-за отсутствия прямой поддержки современных технологий веб-скрейпинга и обработки документов. Несмотря на то что Библиотека объектов HTML Microsoft является мощной, она несколько устарела и может не так гладко обрабатывать современные веб-стандарты, как более новые технологии.

Для сложных задач разбора HTML и веб-скрейпинга часто рекомендуются альтернативные инструменты и языки, такие как Python с библиотеками, например, Beautiful Soup или Scrapy. Эти современные инструменты предлагают большую гибкость, лучшую производительность и более соответствуют текущим веб-стандартам. Однако, работая в экосистеме Microsoft Office, использование VBA с Библиотекой объектов HTML Microsoft остается ценным навыком. Оно открывает прямую манипуляцию с HTML-содержимым способом, который интегрируется бесшовно с приложениями, такими как Excel и Access, предоставляя простой метод для выполнения задач, связанных с базовой обработкой HTML-документов, без необходимости выходить за пределы привычной среды VBA.
