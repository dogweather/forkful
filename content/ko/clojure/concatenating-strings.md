---
title:    "Clojure: 스트링 연결하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 대해 궁금해 할 수 있습니다. Clojure에서 이 기술을 사용하는 이유는 무엇일까요? 문자열을 연결하는 이유는 코드를 더욱 더 읽기 쉽고 이해하기 쉽게 만들고, 여러 문자열을 하나로 만들어서 처리해야 하는 경우 유용하기 때문입니다. 이를 통해 코드의 효율성을 높일 수 있습니다.

## 어떻게

Clojure에서 문자열을 연결하는 방법은 간단합니다. 다음 예제를 살펴보세요.

```Clojure
(def name "Alice")
(def job "programmer")
(def info (str "My name is " name ". I am a " job "."))

(println info)

```

위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
My name is Alice. I am a programmer.
```

위의 예제에서는 `str` 함수를 사용하여 문자열을 연결합니다. 이 함수는 인자로 전달된 모든 문자열을 이어 붙여서 하나의 문자열로 만들어줍니다.

## 딥 다이브

Clojure에서는 문자열을 연결하는 방법이 여러 가지가 있습니다. `str` 함수 이외에도 `format` 함수를 사용할 수 있습니다. `format` 함수는 포맷 문자열과 인자들을 받아서 하나의 문자열로 만들어줍니다. 또한 `StringBuilder`를 사용하여 문자열을 더욱 효율적으로 연결할 수 있습니다.

예를 들어, 다음과 같은 코드를 사용할 수 있습니다.

```Clojure
(def name "Bob")
(def age 25)
(def info (format "My name is %s and I am %s years old." name age))

(println info)

```

위의 코드는 다음과 같은 결과를 출력합니다.

```
My name is Bob and I am 25 years old.
```

위의 예제에서는 `%s`라는 포맷 문자열을 사용했습니다. 이는 문자열로 변환할 인자를 가리킵니다. 따라서 `%s` 앞에 전달된 순서대로 인자가 문자열로 변환되어서 포맷 문자열에 삽입됩니다.

## 관련 링크

- [Clojure 공식 홈페이지](https://clojure.org/)
- [Clojure 문자열 관련 문서](https://clojure.org/reference/strings)