---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що та чому?

Вивід відлагоджування, це процес, завдяки якому ми бачимо стан програми на певних етапах її виконання. Програмісти використовують його щоб легко відслідкувати помилки та покращити код.

## Як це робиться:

```Haskell 
import Debug.Trace

main = trace "Start" $ do 
     let a = 5 
     print a
```

Вивід:

```bash
Start
5
```

Вивід відлагодження (`trace`) вказує, що почався процес. Він також може бути використаний для виведення даних змінної а.

## Поглиблений аналіз

### Історичний контекст

Haskell має багато вбудованих функцій для діагностики та відлагодження, таких як `Debug.Trace`, `Debug.Trace.Observe` та `Debug.Trace.Location`. Вони з'явилися у ранніх версіях Haskell.

### Альтернативи

Ви також можете використовувати `putStrLn` замість `trace`, але ви можете втратити деяку зручність, яку надає `trace`, як-от секції scope та стек-трейси.

### Деталі реалізації

`trace` використовує небезпечні функції `unsafePerformIO` для пропускання виконання IO та виводу в лог без зміни типу. В той час як `putStrLn` використовує тип `IO`, що робить його безпечнішим, але менш гнучким для використання.

## Див. також

1. [Debug.Trace документація](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
2. [Обговорення про Debug.Trace vs. putStrLn](https://stackoverflow.com/questions/23202702/why-does-debug-trace-cause-a-runtime-error-in-the-pure-part-of-a-haskell-functio)
3. [Haskell перехід на безпечніше відлагодження](https://skillsmatter.com/skillscasts/4177-safe-debugging)