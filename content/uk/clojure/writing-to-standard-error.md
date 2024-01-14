---
title:                "Clojure: Писання в стандартну помилку"
simple_title:         "Писання в стандартну помилку"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому

Писання до вихідного потоку стандартної помилки є важливим інструментом для відлагодження вашого програмного коду. Він дозволяє виходити зі звичайного стилю тестування програм та забезпечує більш повний завершений опису помилок, що допомагає розуміти, місце та причину виникнення проблем.

## Як

Для писання до потоку стандартної помилки використовуйте функцію `println!` та напишіть ваше повідомлення у цитатній формі. Наприклад:

```Clojure
(println! "Сталася помилка обробки даних." )
```

Це виведе наступне повідомлення до стандартного потоку помилок:

```
"Сталася помилка обробки даних."
```

## Поглиблене дослідження

Іноді ви можете зіткнутися із ситуацією, коли потрібно вивести більш детальне повідомлення про помилку. В такому разі, ви можете використовувати `prinln!` для виводу форматованої інформації. Наприклад:

```Clojure
(let [x 5
      y 0]
  (try
    (/ x y)
    (catch Exception e
      (println! "Помилка обробки даних:" {:x x :y y} e))))
```

Це виведе наступну інформацію до стандартного потоку помилок:

```
"Помилка обробки даних: {:x 5 :y 0} #error {:cause "Divide by zero" :via [{:type java.lang.ArithmeticException :message "Divide by zero" :at [clojure.lang.Numbers divide "Numbers.java" 197]} {:type java.lang.ArithmeticException :message "Divide by zero" :at [clojure.lang.Numbers divide "Numbers.java" 3742]} {:type java.lang.ArithmeticException :message "Divide by zero" :at [user$eval2400 invokeStatic "form-init2347265261595024914.clj" -1]} {:type java.lang.ArithmeticException :message "Divide by zero" :at [user$eval2400 invoke "form-init2347265261595024914.clj" -1]} {:type clojure.lang.Compiler load "Compiler.java" 7561} {:type clojure.lang.Compiler load "Compiler.java" 7514} {:type clojure.lang.Compiler eval "Compiler.java" 7868} {:type clojure.lang.Compiler eval "Compiler.java" 7880} {:type clojure.core$eval invokeStatic "core.clj" 3206} {:type clojure.core$eval invoke "core.clj" 3202} {:type clojure.main$repl$read_eval_print__7408$fn__7409 invoke "main.clj" 251} {:type clojure.main$repl$read_eval_print__7408 invoke "main.clj" 251} {:type clojure.main$repl$fn__7417 invoke "main.clj" 269} {:type clojure.main$repl invokeStatic "main.clj" 269} {:type clojure.main$repl doInvoke "main.clj" 174} {:type clojure.lang.RestFn invoke "RestFn.java" 1523} {:type user$eval2399 invokeStatic "form-init2347265261595024914.clj" -1} {:type user$eval2399 invoke "form-init2347265261595024914.clj" -1} {:type clojure.lang.Compiler eval "Compiler.java" 7865} {:type clojure.lang.Compiler eval "Compiler.java" 7864} {:type clojure.core