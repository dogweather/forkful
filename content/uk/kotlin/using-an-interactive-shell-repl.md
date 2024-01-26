---
title:                "Використання інтерактивної оболонки (REPL)"
date:                  2024-01-26T04:16:32.274175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що і чому?
REPL (цикл "читати-виконувати-друкувати") — це просте інтерактивне середовище програмування. Програмісти використовують його для швидких випробувань коду, тестування фрагментів або вивчення синтаксису мови без створення повноцінного застосунку.

## Як це зробити:
Запуск REPL Kotlin — це легко. Відкрийте термінал і введіть `kotlinc`. Ви потрапите в оболонку Kotlin. Спробуємо визначити змінну та вивести її значення:

```kotlin
Ласкаво просимо до Kotlin версії 1.7.10 (JRE 1.8.0_292-b10)
Наберіть :help для допомоги, :quit для виходу
>>> val greeting = "Привіт, Kotlin REPL!"
>>> println(greeting)
Привіт, Kotlin REPL!
```

## Поглиблений огляд
REPL Kotlin дебютував разом із мовою, щоб заохотити до експериментів. Він схожий на інтерактивну оболонку Python, але адаптований для синтаксису та особливостей Kotlin. Альтернативи? Інтерактивні середовища в IDE, таких як IntelliJ IDEA, та онлайн-майданчики для Kotlin. REPL працює шляхом компіляції коду "на льоту", забезпечуючи негайний зворотній зв'язок – це життєво важливо для навчання та налагодження.

## Дивіться також
- Документація Kotlin про REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Спробуйте Kotlin у браузері: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Плагін JetBrains Kotlin Playground для IntelliJ IDEA.