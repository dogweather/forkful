---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:12.745194-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0412 VBA \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\
  \ \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u043D\u0435\u043E\u0442\u044A\
  \u0435\u043C\u043B\u0435\u043C\u043E\u0439 \u0447\u0430\u0441\u0442\u044C\u044E\
  \ \u0440\u0435\u0434\u0430\u043A\u0442\u043E\u0440\u0430 Visual Basic (VBE). \u0412\
  \u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0435\u0433\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u0442\u044C: 1. **\u0423\u0441\u0442\u0430\u043D\u043E\u0432\u043A\u0430 \u0442\
  \u043E\u0447\u0435\u043A\u2026"
lastmod: '2024-03-13T22:44:44.751227-06:00'
model: gpt-4-0125-preview
summary: "\u0412 VBA \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A \u044F\u0432\
  \u043B\u044F\u0435\u0442\u0441\u044F \u043D\u0435\u043E\u0442\u044A\u0435\u043C\u043B\
  \u0435\u043C\u043E\u0439 \u0447\u0430\u0441\u0442\u044C\u044E \u0440\u0435\u0434\
  \u0430\u043A\u0442\u043E\u0440\u0430 Visual Basic (VBE)."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0430"
weight: 35
---

## Как использовать:
В VBA отладчик является неотъемлемой частью редактора Visual Basic (VBE). Вот как вы можете его использовать:

1. **Установка точек останова**: Кликните в левом поле рядом с интересующей вас строкой кода или разместите курсор на этой строке и нажмите F9. Это сообщает VBA, что нужно приостановить выполнение, когда выполнение доходит до этой точки.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' Установите точку останова здесь
        Next counter
    End Sub
    ```

    Когда код выполняется, он остановится на строке `Debug.Print counter`, позволяя вам осмотреть значения переменных.

2. **Пошаговое выполнение (F8)**: С помощью этой команды вы выполняете свой код по одному выражению за раз, входя в любые вызванные процедуры. Это полезно для отслеживания взаимодействия вашего кода и функций.

3. **Окно наблюдения**: Используйте Окно наблюдения для мониторинга значений переменных или выражений. Если переменная не в области видимости, Окно наблюдения укажет на это. Кликните правой кнопкой мыши по переменной > Добавить наблюдение.

4. **Немедленное окно (Ctrl+G)**: Это окно особенно полезно для тестирования выражений или изменения значений переменных во время отладки. Введите `?имяПеременной` для вывода текущего значения переменной или присвойте новое значение с помощью `имяПеременной = новоеЗначение`.

    ```vb
    ' В Немедленном окне
    ?counter ' Выводит текущее значение counter
    counter = 3 ' Устанавливает значение counter равным 3
    ```

5. **Пример вывода**:

    Когда вы достигаете точки останова и выполняете команды построчно, используя F8, Немедленное окно может отобразить что-то вроде:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    Здесь мы вручную запросили переменную `counter` после каждой итерации.

## Погружение:
Отладчик в VBA, хотя и надежный, является частью более широкой традиции инструментов отладки в языках программирования, значительно развившихся с момента своего появления. Введенный в первых версиях VBA, он был направлен на предоставление разработчикам простого, но мощного набора инструментов для инспекции кода и его исправления. Со временем улучшения включали условные точки останова, усовершенствованные возможности наблюдения и интеграцию с интерфейсом Excel для более интуитивного осмотра данных.

Однако, по сравнению с современными интегрированными средами разработки (IDE) вроде Visual Studio или Eclipse, средства отладки VBA могут показаться базовыми. Эти современные IDE предлагают более сложные функции, такие как реальное время инспекции переменных, продвинутые точки останова и интегрированные фреймворки для модульного тестирования. Хотя эти альтернативы предоставляют более комплексный опыт отладки, простота и непосредственность отладчика VBA остаются хорошо подходящими для конкретного контекста автоматизации и написания скриптов в приложениях Microsoft Office.

Для программистов, привыкших к этим современным средам, адаптация к инструментам отладки VBA может потребовать изменения подхода. Тем не менее, фундаментальные принципы инспекции переменных, пошагового выполнения кода и наблюдения за поведением во время выполнения универсальны. С практикой отладчик VBA становится незаменимым инструментом для обеспечения безупречной работы ваших автоматизированных скриптов в экосистеме Office.
