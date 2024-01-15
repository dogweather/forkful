---
title:                "표준 에러에 쓰는 방법"
html_title:           "Clojure: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

클로저는 프로그래밍 언어로서 매우 유연하고 강력합니다. 따라서, 개발자는 자신의 코드를 디버깅하고 에러를 처리하기 위해 표준 에러를 출력으로 사용할 수 있습니다. 이는 프로그램의 실행 과정에서 발생하는 여러가지 문제를 파악하고 해결하는데 도움이 됩니다.

## 사용 방법

표준 에러를 출력으로 사용하기 위해서는 `*err*` 변수를 사용해야 합니다. 다음 예제 코드를 확인해보세요.

```Clojure
(defn calculate [x y]
  (if (zero? y)
    (throw (Exception. "Divide by zero error"))
    (/ x y)))

(defn -main []
  (let [x (read-line)
        y (read-line)]
    (try
      (println (calculate (Integer/parseInt x) (Integer/parseInt y)))
      (catch Exception e
        (prn "Error: " (.getMessage e)
             "occurred at line " (.getStackTrace e 0))))))

```

위의 코드는 사용자로부터 두 수를 입력받아 나눗셈을 수행하는 함수를 정의한 후 `try-catch` 블록을 사용하여 에러를 처리합니다. 결과적으로, 만약 `y`값이 `0`이라면 `catch` 블록이 실행되어 에러 메시지와 해당 에러가 발생한 라인을 출력합니다.

```
5
2
2.5
```
## 딥 다이브

이번에는 표준 에러를 출력하지 않고, 에러를 전파하는 방법에 대해 알아보겠습니다. `try-catch` 블록에서 `catch` 블록 대신 `finally` 블록을 사용하여 에러를 전파할 수 있습니다. 또한, `printStackTrace` 함수를 사용하여 디버깅 정보를 더 자세히 출력할 수도 있습니다. 다음 예제 코드를 확인해보세요.

```Clojure
(defn calculate [x y]
  (if (zero? y)
    (throw (Exception. "Divide by zero error"))
    (/ x y)))

(defn -main []
  (let [x (read-line)
        y (read-line)]
    (try
      (println (calculate (Integer/parseInt x) (Integer/parseInt y)))
      (finally
        (printStackTrace)))))

```

위의 코드는 `finally` 블록에서 `printStackTrace` 함수를 호출하여 디버깅 정보를 출력하도록 설정합니다. 결과적으로, `divide-by-zero` 에러가 발생하면 해당 에러의 전체 스택 트레이스가 출력됩니다.

```
5
0

#<ArithmeticException java.lang.ArithmeticException: Divide by zero error>
   clojure.lang.Numbers.divide (Numbers.java:159)
   ... (중략)
```

## 더 보기

- https://clojure.org/
- https://clojure.org/api/cheatsheet