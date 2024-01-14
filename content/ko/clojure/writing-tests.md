---
title:    "Clojure: 테스트 작성하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 왜 당신은 테스트를 작성해야 할까요?

테스트를 작성하는 것은 개발 과정에서 매우 중요합니다. 이를 통해 코드를 더욱 견고하고 신뢰할 수 있게 만들 수 있으며, 버그를 미리 발견하고 수정할 수 있습니다. 또한 테스트를 작성함으로써 코드의 유지보수성을 높일 수 있습니다.

## 어떻게 하나요?

Clojure에서 테스트를 작성하는 것은 매우 쉽습니다. 우리는 `clojure.test` 라이브러리를 사용하여 테스트를 만들 수 있습니다.

```Clojure
(ns my-namespace.core-test
  (:require [clojure.test :refer :all]
            [my-namespace.core :refer :all]))

;; 테스트 케이스 생성
(deftest addition-test
  (is (= (add 2 3) 5))
  (is (= (add 10 5) 15)))

;; 전체 테스트 실행
(run-tests)
```

위에서는 `is` 매크로를 사용하여 예상 결과값과 실제 결과값을 비교하였습니다. `deftest` 매크로를 사용하여 테스트 케이스를 생성하고, `run-tests` 함수를 사용하여 전체 테스트를 실행합니다.

## 깊이 들어가보기

테스트를 작성할 때는 코드의 모든 경로를 테스트하는 것이 중요합니다. 이를 위해 Clojure에서는 `testing` 매크로를 사용할 수 있습니다.

```Clojure
(ns my-namespace.core-test
  (:require [clojure.test :refer :all]
            [my-namespace.core :refer :all]))

(deftest addition-test
  (testing "2와 3을 더한 결과는 5가 되어야 합니다."
    (is (= (add 2 3) 5)))
  (testing "10과 5를 더한 결과는 15가 되어야 합니다."
    (is (= (add 10 5) 15))))
```

위 예제에서는 `testing` 매크로를 사용하여 각 테스트 케이스를 더욱 명확하게 구분하였습니다. 테스트를 작성할 때는 `testing` 매크로를 사용하여 코드의 모든 경로를 테스트하는 것을 권장합니다.

## 참고 자료

- [ClojureDocs: clojure.test](https://clojuredocs.org/clojure.test)
- [Clojure for Machine Learning: Writing Tests](https://clojure.org/guides/testing)
- [How to Test in Clojure](https://luminusweb.com/docs/testing.html)

# 더 보기

- [Clojure 공식 사이트](https://clojure.org/)
- [Clojure 한국 사용자 그룹](https://clojurekr.github.io/)
- [온라인 Clojure 공부 자료 모음](https://practicalli.github.io/)