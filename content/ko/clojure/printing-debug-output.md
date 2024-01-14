---
title:    "Clojure: 디버그 출력 출력"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프린팅하는 이유는 문제가 발생한 코드를 이해하고 디버깅하는 것에 도움을 줄 수 있기 때문입니다.

## 어떻게

디버그 출력을 프린팅하려면 `println` 함수를 사용하면 됩니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Clojure
(defn calculate-sum
    "두 숫자의 합을 계산하는 함수"
    [num1 num2]
    (println "num1의 값은" num1)
    (println "num2의 값은" num2)
    (+ num1 num2))

(calculate-sum 5 3)
```

위 코드는 `calculate-sum` 함수를 정의하고 그 안에서 `println` 함수를 사용하여 디버그 출력을 프린팅하는 예시입니다. 아래는 해당 코드의 출력결과입니다.

```Clojure
num1의 값은 5
num2의 값은 3
```

## 딥 다이브

디버그 출력은 코드를 디버깅하는 과정에서 매우 유용합니다. `println` 함수를 사용하여 변수의 값이나 코드의 실행 흐름을 확인할 수 있습니다. 또한, `pr` 함수를 사용하여 인자를 더 자세하게 출력할 수도 있습니다. 예를 들어, `pr` 함수는 스트링이 아닌 자료형을 출력할 때 더 적합한 포맷으로 출력해줍니다.

```Clojure
(defn calculate-product
    "두 숫자의 곱을 계산하는 함수"
    [num1 num2]
    (pr "num1의 값은" num1)
    (pr "num2의 값은" num2)
    (* num1 num2))

(calculate-product 2 4)
```

위 코드는 `pr` 함수를 사용하여 변수의 값과 계산식을 더 세부적으로 출력하는 예시입니다. 아래는 해당 코드의 출력결과입니다.

```Clojure
num1의 값은2
num2의 값은4
```

## 관련 정보

- [Clojure 공식 문서 - 입출력](https://clojure.org/guides/io)
- [Clojure 공식 문서 - Debugging](https://clojure.org/guides/debugging)