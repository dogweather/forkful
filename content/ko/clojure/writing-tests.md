---
title:                "Clojure: 코딩 테스트 작성"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성할까요?

테스트는 프로그래머들에게 코드의 신뢰성을 높이고 버그를 더 빨리 발견할 수 있는 중요한 도구입니다. 그렇기 때문에 테스트를 작성함으로써 코드를 더 나은 품질로 개선할 수 있습니다.

## 테스트 작성하는 방법

테스트 코드를 작성하는 것은 그렇게 어려운 일이 아닙니다. 먼저 일반적인 Clojure 함수를 작성하는 것처럼 `defn` 문법을 사용하여 테스트 함수를 정의합니다. 예를 들어, `utils.test` 네임스페이스에 `add` 함수를 테스트하는 함수는 다음과 같이 작성할 수 있습니다:

```Clojure
(defn add-test []
  (is (= 4 (add 2 2)))
  (is (= 8 (add 3 5))))
```

위의 예제에서는 `is` 매크로를 사용하여 예상된 결과와 실제 결과를 비교합니다. 만약 테스트 함수에서 예상한 결과와 실제 결과가 다를 경우는 `Clojure.lang.ExceptionInfo` 예외를 던집니다.

테스트를 실행하려면, `clojure.test/run-tests` 함수를 사용하면 됩니다. 우리의 `add-test` 함수를 테스트하려면, 다음과 같이 실행할 수 있습니다:

```Clojure
(clojure.test/run-tests 'utils.test)
```

위의 코드는 `utils.test` 네임스페이스에 정의된 모든 테스트를 실행합니다. 모든 테스트가 성공하면, 다음과 같은 출력을 볼 수 있습니다:

```
Testing utils.test
Ran 1 tests containing 2 assertions.
0 failures, 0 errors.
```

테스트를 작성할 때는 함수의 각 분기점에서 예상하는 결과와 실제 결과를 비교하는 것이 중요합니다. 더 복잡한 테스트는 다른 데이터 형식을 나타내는 값들을 조합하는 방식으로 작성할 수 있습니다.

## 깊이 파고들기

테스트를 작성할 때는 인터넷에서 검색하여 도움을 받을 수 있습니다. Clojure 프로그래밍을 공부하면서 많은 예제 코드를 볼 수 있으며, 이 예제 코드들은 테스트를 작성하는 데에도 도움이 됩니다. 또한 당신의 코드에 대한 테스트 환경을 구성해서 자동으로 테스트를 실행할 수 있도록 할 수도 있습니다. 이를 통해 버그를 더 빠르게 발견할 수 있으며, 코드를 더 신뢰성있게 개발할 수 있습니다.

## 더 알아보기

- [Clojure 테스트 라이브러리 가이드](https://clojure.org/guides/test)
- [Clojure 테스트 예제 코드 컬렉션](https://github.com/clojure/test.check)
- [Clojure의 자동 테스트 환경 설정하기](https://github.com/clojure/test.check)
- [Clojure 테스트 문서](https://clojure.github.io/clojure/clojure.test-api.html)

## 더 알아보기