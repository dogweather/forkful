---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:19.061521-07:00
description: "\uBC29\uBC95: Clojure\uB294 JVM\uC744 \uD65C\uC6A9\uD558\uC5EC \uB2E4\
  \uC591\uD55C \uD14C\uC2A4\uD305 \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uC9C0\uC6D0\
  \uD569\uB2C8\uB2E4. \uADF8\uB7EC\uB098 \uC77C\uBC18\uC801\uC73C\uB85C \uC0AC\uC6A9\
  \uB418\uB294 \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 `clojure.test`\uC785\
  \uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.666079-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 JVM\uC744 \uD65C\uC6A9\uD558\uC5EC \uB2E4\uC591\uD55C \uD14C\
  \uC2A4\uD305 \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uC9C0\uC6D0\uD569\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

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
