---
title:                "테스트 작성하기"
aliases:
- ko/clojure/writing-tests.md
date:                  2024-02-03T19:30:19.061521-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Clojure에서 테스트를 작성하는 것은 다른 프로그래밍 언어에서처럼, 주 코드베이스가 예상대로 작동하는지 검증하기 위해 전용 코드를 생성하는 과정을 포함합니다. 이는 정확성을 보장하고 리팩토링을 용이하게 하며 코드 안정성을 향상시키는 데 도움이 됩니다.

## 방법:
Clojure는 JVM을 활용하여 다양한 테스팅 프레임워크를 지원합니다. 그러나 일반적으로 사용되는 내장 라이브러리는 `clojure.test`입니다. 다음은 간단한 예입니다:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "덧셈 기능"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
이 테스트를 실행한 후, 다음과 비슷한 출력을 볼 수 있습니다:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

더 많은 기능을 원하는 사용자의 경우, `Midje`나 `test.check`와 같은 서드파티 라이브러리를 활용할 수 있습니다. 다음은 Midje를 사용한 비슷한 테스트 방법입니다:

먼저, project.clj 의존성에 Midje를 추가합니다:
```clojure
[midje "1.9.9"]
```

그런 다음, Midje로 테스트를 다음과 같이 작성할 수 있습니다:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "덧셈 테스트"
  (add 2 2) => 4
  (add 3 4) => 7)
```

`lein midje`를 통해 테스트를 실행하면, 다음과 비슷한 출력이 표시됩니다:

```
모든 검사(2개)가 성공했습니다.
```
