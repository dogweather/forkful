---
title:                "Вывод отладочной информации"
aliases:
- ru/haskell/printing-debug-output.md
date:                  2024-01-29T00:00:45.010544-07:00
model:                 gpt-4-0125-preview
simple_title:         "Вывод отладочной информации"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Вывод отладочной информации - это вывод данных из программы для понимания того, что происходит под капотом. Программисты делают это для отслеживания переменных, понимания потока выполнения и устранения надоедливых ошибок.

## Как:

Простой способ вывода отладочной информации в Haskell - это использование функции `print`, которая принимает значение, являющееся экземпляром типового класса `Show`, и выводит его в консоль.

```Haskell
main :: IO ()
main = do
  let number = 42
  print number
  putStrLn "Отладка в Haskell - это проще простого!"

-- Вывод:
-- 42
-- Отладка в Haskell - это проще простого!
```

Для более сложных структур данных убедитесь, что они выводят `Show` для включения красивой печати:

```Haskell
data Cake = Chocolate | Vanilla deriving Show

debugFlavor :: Cake -> IO ()
debugFlavor flavor = print flavor

main :: IO ()
main = debugFlavor Chocolate

-- Вывод:
-- Chocolate
```

Иногда нам нужна временная отладка, которую легко удалить позже. Введите модуль `Debug.Trace`.

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ trace "Это напечатается первым" "Это напечатается вторым"

-- Вывод:
-- Это напечатается первым
-- Это напечатается вторым
```

Функция `trace` печатает строку, когда значение оценивается, но это побочный эффект в чистой части кода. Это полезно, но используйте с осторожностью!

## Глубже

В древние времена отладка, возможно, была старым добрым "трюком с печатью". Haskell предлагает это с функциональным изворотом и инструментами для более чистых практик отладки. Введите `print` и модуль `Debug.Trace`, как мы уже рассмотрели.

Альтернативы `print` включают `putStrLn` для строк и `putStr`, если вам не нравится автоматический перевод строки. У `Debug.Trace` также есть варианты, такие как `traceShow`, которые работают напрямую с экземплярами `Show`, избавляя вас от вызова `show`.

Что касается деталей реализации, `print` по существу является `putStrLn . show`. Она печатает любые данные, которые можно показать (`Show`), в stdout. Функции `Debug.Trace`, с другой стороны, предназначены для временного использования во время разработки. Они украдкой встроены в чистый код и нарушают ссылочную прозрачность, что в долгосрочной перспективе является табу.

Не забывайте и о библиотеках логирования для серьезных приложений, которые предлагают больше контроля и меньше "отладки печатью".

## Смотрите также

- Документация по `Debug.Trace`: [https://hackage.haskell.org/package/base/docs/Debug-Trace.html](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- Haskell Wiki по отладке: [https://wiki.haskell.org/Debugging](https://wiki.haskell.org/Debugging)
- Хорошее обсуждение, почему не стоит использовать `Debug.Trace` и что делать вместо этого: [https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice](https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice)
