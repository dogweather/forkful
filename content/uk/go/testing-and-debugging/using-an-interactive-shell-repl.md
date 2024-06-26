---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:38.295982-07:00
description: "\u042F\u043A \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0442\
  \u0438\u0441\u044F: \u0425\u043E\u0447\u0430 Go \u043D\u0435 \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E\
  \ REPL, \u0441\u043F\u0456\u043B\u044C\u043D\u043E\u0442\u0430 \u0441\u0442\u0432\
  \u043E\u0440\u0438\u043B\u0430 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\
  \u0442\u0438 \u043D\u0430 \u043A\u0448\u0442\u0430\u043B\u0442 `gore`, \u0449\u043E\
  \u0431 \u0437\u0430\u043F\u043E\u0432\u043D\u0438\u0442\u0438 \u0446\u044E \u043F\
  \u0440\u043E\u0433\u0430\u043B\u0438\u043D\u0443. \u0421\u043F\u043E\u0447\u0430\
  \u0442\u043A\u0443 \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:48.437314-06:00'
model: gpt-4-0125-preview
summary: "\u0425\u043E\u0447\u0430 Go \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\
  \u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E REPL,\
  \ \u0441\u043F\u0456\u043B\u044C\u043D\u043E\u0442\u0430 \u0441\u0442\u0432\u043E\
  \u0440\u0438\u043B\u0430 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\
  \u0438 \u043D\u0430 \u043A\u0448\u0442\u0430\u043B\u0442 `gore`, \u0449\u043E\u0431\
  \ \u0437\u0430\u043F\u043E\u0432\u043D\u0438\u0442\u0438 \u0446\u044E \u043F\u0440\
  \u043E\u0433\u0430\u043B\u0438\u043D\u0443."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0456\
  \u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0457 \u043E\u0431\
  \u043E\u043B\u043E\u043D\u043A\u0438 (REPL)"
weight: 34
---

## Як користуватися:
Хоча Go не включає вбудованого REPL, спільнота створила інструменти на кшталт `gore`, щоб заповнити цю прогалину. Спочатку встановіть `gore`, запустивши:

```
$ go get -u github.com/motemen/gore
```

Після встановлення запустіть `gore`, набравши `gore` у терміналі:

```
$ gore
```

Ви повинні побачити запрошення, готове приймати команди Go. Спробуймо простий приклад:

```
gore> :import fmt
gore> fmt.Println("Привіт, Go REPL!")
```

Ви побачите вивід на кшталт:

```
Привіт, Go REPL!
```

Змінні та визначення функцій працюють як очікується. Ви можете оголосити функцію:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Площа круга з радіусом 4:", areaCircle(4))
```

І одразу отримати результат:

```
Площа круга з радіусом 4: 50.26548245743669
```

## Глибше занурення:
Концепція REPL є давньою, вона сягає до машин Lisp 1960-х років, забезпечуючи інтерактивний досвід програмування. На відміну від мов, таких як Python або JavaScript, Go було розроблено без REPL, зосереджуючись замість цього на скомпільованих бінарниках задля продуктивності та простоти. Це відображає філософію Go щодо простоти та його розробку для масштабованого та стійкого програмного забезпечення.

Однак такі інструменти, як `gore` або `goplay`, демонструють винахідливість спільноти Go у заповненні цього прогапу. Ці інструменти динамічно аналізують код Go та використовують пакет `go/eval` або подібні механізми для його виконання в реальному часі, хоча і з деякими обмеженнями порівняно з рідним середовищем REPL. Ці обмеження випливають з типової системи Go та моделі компіляції, які можуть ускладнити виконання на льоту.

Хоча середовища REPL надзвичайно корисні для освіти та швидких тестів, екосистема Go зазвичай орієнтується на традиційні процеси компіляції-запуску для більшості завдань розробки. Інтегровані середовища та редактори з підтримкою Go, такі як Visual Studio Code або GoLand, пропонують інтегровані інструменти для тестування та відлагодження, що значною мірою знімають потребу в REPL для професійної розробки.

Однак для дослідницького програмування, прототипування або навчання REPL, на кшталт `gore`, пропонують цінну альтернативу, дозволяючи програмістам, звиклим до REPL в інших мовах, насолоджуватися подібним досвідом в Go.
