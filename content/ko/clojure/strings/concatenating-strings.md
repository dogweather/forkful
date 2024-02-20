---
date: 2024-01-20 17:34:43.496747-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC11C\uB85C \uB2E4\uB978 \uBB38\
  \uC790\uC5F4\uB4E4\uC744 \uBD99\uC5EC\uC11C \uD558\uB098\uC758 \uBB38\uC790\uC5F4\
  \uB85C \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC870\uD569\uD558\uAC70\uB098 \uCD9C\uB825\
  \ \uD615\uC2DD\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.590998
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC11C\uB85C \uB2E4\uB978 \uBB38\uC790\
  \uC5F4\uB4E4\uC744 \uBD99\uC5EC\uC11C \uD558\uB098\uC758 \uBB38\uC790\uC5F4\uB85C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uB370\uC774\uD130\uB97C \uC870\uD569\uD558\uAC70\uB098 \uCD9C\uB825 \uD615\
  \uC2DD\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결은 서로 다른 문자열들을 붙여서 하나의 문자열로 만드는 것입니다. 프로그래머들은 데이터를 조합하거나 출력 형식을 개선하기 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
Clojure에서 문자열을 연결하는 기본적인 방법은 `str` 함수를 사용하는 것입니다. 다음은 간단한 예제들입니다:

```Clojure
(str "Hello," " world!") ; => "Hello, world!"

; 숫자와 문자열을 함께 연결하고 싶을 때
(str "The answer is " 42) ; => "The answer is 42"

; 컬렉션의 문자열들을 연결할 때
(apply str ["Clojure" " is" " awesome"]) ; => "Clojure is awesome"
```

위 예제들에서 보이듯이, `str` 함수를 이용해 여러 문자열을 순서대로 결합하여 새로운 문자열을 만들 수 있습니다. 숫자나 다른 데이터 타입도 `str` 함수와 함께 문자열로 변환되어 연결됩니다.

## Deep Dive (심층 분석)
Clojure에서 문자열을 연결하는 작업은 Java의 `StringBuilder`를 통해 효율적으로 수행됩니다. 이 방법은 문자열을 하나의 큰 메모리 블럭으로 관리하여 여러 개의 작은 문자열들을 합치는 과정에서 발생할 수 있는 메모리 낭비와 시간 소모를 줄여줍니다.

역사적으로 봤을 때, 문자열 연결은 프로그래밍에서 늘 중요한 요소였고 대부분의 프로그래밍 언어들은 이를 지원합니다. Clojure는 JVM(Java Virtual Machine) 위에서 동작하기 때문에 Java의 효율적인 문자열 처리 능력을 그대로 활용할 수 있다는 장점이 있습니다.

만약 성능에 민감하거나 큰 데이터를 다룬다면, `clojure.core` 라이브러리의 `str` 대신 `StringBuilder`를 직접 사용할 수도 있습니다. 그러나 일반적인 사용에서는 `str` 함수가 간결하고 쉬운 선택입니다.

Clojure에는 또 다른 문자열 연결 방법으로 `format` 함수가 있습니다. 이 함수는 Java의 `String.format`과 유사하게, 포맷 스트링을 사용하여 복잡한 형식의 문자열을 조립할 수 있게 도와줍니다.

## See Also (더 알아보기)
- Clojure 공식 문서의 `str` 함수: https://clojuredocs.org/clojure.core/str
- 문자열 조작에 대한 좀 더 깊은 이해가 필요하면 ClojureDocs가 유용할 것입니다: https://clojuredocs.org/clojure.string/join
- Java의 `StringBuilder` 클래스에 대한 자세한 정보는 공식 Java 문서에서: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
- `format` 함수 사용법: https://clojuredocs.org/clojure.core/format
