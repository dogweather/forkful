---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:46.916091-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0417\u0430\u043F\u0443\u0441\u0442\u0438\u0442\u044C REPL Kotlin\
  \ \u043E\u0447\u0435\u043D\u044C \u043B\u0435\u0433\u043A\u043E. \u041E\u0442\u043A\
  \u0440\u043E\u0439\u0442\u0435 \u0442\u0435\u0440\u043C\u0438\u043D\u0430\u043B\
  \ \u0438 \u043D\u0430\u0431\u0435\u0440\u0438\u0442\u0435 `kotlinc`. \u0412\u044B\
  \ \u043F\u043E\u043F\u0430\u0434\u0435\u0442\u0435 \u0432 \u043E\u0431\u043E\u043B\
  \u043E\u0447\u043A\u0443 Kotlin. \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\
  \u043E\u043F\u0440\u043E\u0431\u0443\u0435\u043C \u043E\u043F\u0440\u0435\u0434\u0435\
  \u043B\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.981606-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0443\u0441\u0442\u0438\u0442\u044C REPL Kotlin \u043E\
  \u0447\u0435\u043D\u044C \u043B\u0435\u0433\u043A\u043E."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0438\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0439\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0438 (REPL)"
weight: 34
---

## Как это сделать:
Запустить REPL Kotlin очень легко. Откройте терминал и наберите `kotlinc`. Вы попадете в оболочку Kotlin. Давайте попробуем определить переменную и вывести ее значение:

```kotlin
Добро пожаловать в Kotlin версии 1.7.10 (JRE 1.8.0_292-b10)
Введите :help для помощи, :quit для выхода
>>> val greeting = "Привет, Kotlin REPL!"
>>> println(greeting)
Привет, Kotlin REPL!
```

## Подробнее
REPL Kotlin был представлен вместе с языком, чтобы стимулировать экспериментирование. Он похож на интерактивную оболочку Python, но адаптирован под синтаксис и особенности Kotlin. Альтернативы? Интерактивные среды в средах разработки, таких как IntelliJ IDEA, и онлайн-площадки Kotlin. REPL работает путем компиляции кода на лету, обеспечивая мгновенную обратную связь, что критически важно для обучения и отладки.

## Смотрите также
- Документация Kotlin о REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Попробуйте Kotlin в браузере: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Плагин JetBrains Kotlin Playground для IntelliJ IDEA.
