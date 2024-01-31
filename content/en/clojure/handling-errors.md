---
title:                "Handling errors"
date:                  2024-01-21T21:19:31.756377-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Error handling is about managing the unexpected in programs—like a bouncer dealing with troublemakers. Programmers like it smooth; error handling helps keep the trouble in line, ensuring their code doesn't trip and fall when faced with the unexpected.

## How to:
Clojure, like its Lisp ancestors, leans on exceptions for dealing with errors. Here’s how you show what you’re made of when things go south.

Throwing an exception is straightforward:
```Clojure
(throw (Exception. "Oops! Something went wrong."))
```

Catching an exception, you’ll be doing this a lot:
```Clojure
(try
  ;; risky code
  (/ 1 0)
  (catch ArithmeticException e
    (println "Can't divide by zero!"))
  ;; finally block runs no matter what
  (finally 
    (println "Clean-up code goes here.")))
```
Sample output for the catch block above:
```
Can't divide by zero!
Clean-up code goes here.
```

Using `ex-info` and `ex-data` for richer context about exceptions:
```Clojure
(try
  ;; causing a custom exception
  (throw (ex-info "Custom error" {:type :custom-failure}))
  (catch Exception e
    ;; getting the data out of our custom exception
    (println (ex-data e))))
```
Sample output:
```
{:type :custom-failure}
```

## Deep Dive
The error handling story in Clojure isn't radically different from other Lisps or even Java (from which it inherits the `try-catch` mechanism). It's pragmatic; using exceptions is the main path, just like Java, but Clojure offers a functional flavor with `ex-info` and `ex-data` for richer error data.

Alternatives for error handling in Clojure include using monadic constructs, such as the `either` monad from libraries like `cats`, or core.async for channel-based error propagation. However, these are more complex and used in specific scenarios.

Historically, error handling in programming languages has evolved from simple status returns to the more sophisticated exception handling mechanisms of modern languages. Clojure opts for simplicity and a touch of functional programming, blending the old and the new.

## See Also
- Clojure's guide to exceptions: https://clojure.org/guides/exceptions
- “Cats” library for more functional approaches: https://github.com/funcool/cats
- “Core.async” for asynchronous programming: https://github.com/clojure/core.async
