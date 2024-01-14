---
title:    "Clojure: 디버그 출력 출력하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜
디버그 출력을 사용해야할까요? 일반적으로 디버그 출력은 코드를 디버그하고 오류를 찾는 데 도움이됩니다.

## 어떻게 하나요?
디버그 출력을 사용하려면 `println` 함수를 사용합니다.
```Clojure
(println "Hello, World!")
```

출력:
```
Hello, World!
```

## 깊이 빠져보기
디버그 출력은 주로 코드에서 변수의 값을 확인하거나 코드의 흐름을 따르는 데 사용됩니다. 이를 통해 오류가 발생하는 위치를 찾을 수 있고, 코드를 수정할 때 문제를 파악하는 데 도움이 됩니다.

또한, `prn` 함수를 사용하면 디버그 출력을 더 깔끔하게 나타낼 수 있습니다. 예를 들어, `prn` 함수는 자동으로 따옴표를 삽입해주기 때문에 변수의 값을 따로 처리할 필요가 없습니다.
```Clojure
(def num 10)
(prn "The value of num is" num)
```

출력:
```
"The value of num is" 10
```

## 또 다른 예
디버그 출력을 사용하는 다른 예는 함수의 입력값과 출력값을 확인하는 것입니다. 예를 들어, 아래의 `square` 함수는 주어진 숫자를 제곱한 값을 반환합니다. 디버그 출력을 사용하여 함수의 입력값과 출력값을 확인할 수 있습니다.
```Clojure
(defn square [num]
  (prn "Input:" num)
  (let [result (* num num)]
    (prn "Output:" result)
    result))
    
(square 4)
```

출력:
```
"Input:" 4
"Output:" 16
16
```

## 참고할 만한 링크들
- [Clojure 디버깅 가이드](https://www.braveclojure.com/debugging/)
- [Clojure 소개](https://clojure.org/about/introduction)
- [Clojure REPL (Read-Eval-Print-Loop) 사용하기](https://lispcast.com/clojure-repl/)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)