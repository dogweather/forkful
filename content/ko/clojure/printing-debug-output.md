---
title:                "Clojure: 디버그 출력을 인쇄하기"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

 프로그래밍을 할 때, 디버그 출력을 하는 이유는 자신의 코드를 디버깅하기 위해서입니다.

## 하는 방법

 디버그 출력은 코드를 디버깅하는 데 도움이 되는 유용한 도구입니다. 다음은 Clojure에서 디버그 출력을 하는 방법과 예시입니다.

```Clojure
(defn add [a b]
  (println "덧셈을 시작합니다.")
  (println "현재 a의 값:" a)
  (println "현재 b의 값:" b)
  (let [result (+ a b)]
    (println "덧셈 결과:" result)
    result))

(add 1 2)
```

위 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.
```
덧셈을 시작합니다.
현재 a의 값: 1
현재 b의 값: 2
덧셈 결과: 3
```

위 예시에서는 `println` 함수를 사용하여 변수들의 현재 값을 출력하고, 결과값을 확인할 수 있습니다.

## 깊이 분석

따로 설정하지 않아도 Clojure에서는 이미 `println` 함수를 사용하여 디버그 출력을 할 수 있습니다. 하지만 더 나은 출력 결과를 얻기 위해서는 다른 도구들을 사용할 수도 있습니다. 예를 들어, `clojure.tools.logging` 라이브러리를 사용하면 로그 레벨을 설정할 수 있고, 더 많은 정보를 출력할 수 있습니다.

## 더 알아보기

- [Clojure 공식 문서: 디버깅](https://clojure.org/guides/repl/debugging)
- [Clojure 공식 문서: 로깅](https://clojure.org/reference/logging)
- [Clojure tools.logging 라이브러리](https://github.com/clojure/tools.logging)

## 참고

이 글은 [Clojure 디버깅하기](https://codeburst.io/clojure-debugging-d5c5d87bb99c)를 참고하여 작성되었습니다.