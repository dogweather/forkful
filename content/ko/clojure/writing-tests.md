---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
테스트 코딩은 코드가 올바르게 동작하는지 확인하기 위한 과정입니다. 프로그래머는 예상치 못한 오류를 방지하고, 코드의 품질을 보증하기 위해 테스트를 작성합니다.

## 작성 방법:
Clojure에서 테스트를 작성하려면 `clojure.test` 라이브러리를 사용합니다.

```Clojure
(require '[clojure.test :refer [deftest is testing]])

(deftest test-addition
  (testing "기본 덧셈 기능"
    (is (= 4 (+ 2 2)))
    (is (= 7 (+ 3 4)))))

(clojure.test/run-tests)
```

위 코드는 두 개의 덧셈 테스트를 수행하며, 테스트 결과는 다음과 같습니다.

```Clojure
{:test 2, :pass 2, :fail 0, :error 0, :type :summary}
```

## 깊이 알아보기:
Clojure의 테스트 프레임워크는 JUnit 같은 자바의 테스팅 도구와 같은 역할을 합니다. 다른 대안으로는 Midje나 Speclj 같은 도구가 있지만 `clojure.test`는 Clojure 코어에 포함되어 있어서 별도의 설치 없이 사용할 수 있습니다. 테스트 함수 안에서는 `is` 매크로를 사용하여 실제 값과 예상 값을 비교합니다.

## 참고자료:
- Clojure 공식 가이드: [Clojure - Functional Programming for the JVM](https://clojure.org/)
- `clojure.test` 사용법: [Clojure Test Framework](https://clojure.github.io/clojure/clojure.test-api.html)
- 대안적인 테스트 라이브러리:
  - [Midje on GitHub](https://github.com/marick/Midje)
  - [Speclj on GitHub](https://github.com/slagyr/speclj)
