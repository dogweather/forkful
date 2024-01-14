---
title:    "Clojure: 문자열을 소문자로 변환하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 대해 궁금하신가요? 이 글은 Clojure에서 문자열을 소문자로 변환하는 방법에 대해 알려드릴 것입니다.

## 어떻게

문자열을 소문자로 변환하는 것은 간단한 작업입니다. 가장 쉬운 방법은 `lower-case` 함수를 사용하는 것입니다. 예제를 살펴보겠습니다.

```Clojure
(lower-case "HELLO WORLD")
```

```Clojure
=> "hello world"
```

문자열의 길이나 문자의 종류에 상관없이 모두 소문자로 변환됩니다. 이 함수는 재귀적으로 동작하며 리스트 내의 모든 문자열도 소문자로 변환할 수 있습니다. 예를 들어:

```Clojure
(lower-case ["HELLO" "WORLD"])
```

```Clojure
=> ["hello" "world"]
```

하지만 이런 방법 외에도 다른 방법들이 존재합니다. 예를 들어, `clojure.string` 라이브러리에는 `lower-case`가 포함된 몇 가지 함수가 있습니다. 이 함수들은 문자열 처리에 유용할 수 있습니다. 아래 코드는 `clojure.string` 에서 제공하는 `lower-case` 함수를 사용해 입력 문자열을 소문자로 변환하는 예제입니다.

```Clojure
(require '[clojure.string :as str])
(str/lower-case "HELLO WORLD")
```

```Clojure
=> "hello world"
```

위 예제에서는 `require`를 사용해 `clojure.string` 를 로드하고, `str` 별칭을 사용해 `clojure.string` namespace를 참조하는 방법을 보여줍니다.

## 깊이 파고들기

마지막으로, `lower-case` 함수의 내부 동작 원리를 살펴보겠습니다.

Clojure는 기본적으로 모든 문자열을 중첩된 문자 리스트로 다루며, `lower-case` 함수는 이 문자 리스트를 재귀적으로 순회하면서 각 문자를 소문자로 변환합니다. 이를 통해 전체 문자열의 소문자로 변환된 결과를 반환합니다.

## 참고

- [ClojureDocs: lower-case](https://clojuredocs.org/clojure.core/lower-case)
- [ClojureDocs: clojure.string](https://clojuredocs.org/clojure.string)

# 이미지 함수

이 기사에서는 Clojure에서 이미지 함수를 사용하는 방법에 대해 알아보았습니다. 이미지 처리 및 편집에 유용한 다양한 함수들이 있으니 참고해보세요!