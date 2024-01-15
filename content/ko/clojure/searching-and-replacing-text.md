---
title:                "텍스트 검색과 대체"
html_title:           "Clojure: 텍스트 검색과 대체"
simple_title:         "텍스트 검색과 대체"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 대체하는 작업은 일상적으로 우리가 많이 하는 일입니다. 예를 들어, 오타를 고치거나 특정한 패턴의 문자열을 한번에 바꾸기 위해서 등 다양한 이유가 있을 수 있습니다. Clojure를 사용하면 텍스트를 쉽고 빠르게 검색하고 대체할 수 있으며, 더 나은 코드를 쓰는 방법을 배울 수 있습니다.

## 어떻게

코딩 예제와 함께 설명하는 방식으로 진행하겠습니다. Clojure REPL에 다음과 같은 코드를 입력하면, 텍스트를 검색하고 대체하는 방법을 쉽게 이해할 수 있습니다.

```Clojure
(def text "Hello world, I love Clojure!")
(clojure.string/replace text "love" "like")
```

위 코드는 `clojure.string/replace` 함수를 사용하여 "Hello world, I like Clojure!"라는 결과를 출력합니다. 이 코드에서 `clojure.string/replace` 함수는 첫 번째 인자로 전체 텍스트를, 두 번째 인자로 검색할 문자열을, 세 번째 인자로 대체할 문자열을 받습니다. 위 예제에서는 "love"라는 문자열을 "like"로 바꾸는 것을 볼 수 있습니다.

## 깊게 들어가기

검색과 대체를 하기 전에, 텍스트를 조작하기 전에는 항상 유니코드 문자열로 변환하는 것이 좋습니다. 그렇지 않으면 예상치 못한 결과가 발생할 수 있습니다. Clojure에서는 `clojure.string/unicode` 함수를 사용하여 문자열을 유니코드로 변환할 수 있습니다.

또한, `clojure.string/replace` 함수는 정규식을 사용하여 문자열을 검색할 수도 있습니다. 정규식은 복잡한 패턴을 사용하여 텍스트를 검색하고 대체하는 방법을 제공합니다. 예를 들어, `clojure.string/replace` 함수의 세 번째 인자로 정규식을 사용할 수 있으며, 이를 통해 더 많은 옵션을 설정할 수 있습니다.

## 연관된 내용

마지막으로 "연관된 내용"이라는 마크다운 글자로 된 부분이 있습니다. 여기에는 Clojure 공식 사이트 또는 다른 유용한 자료들을 링크로 추가할 수 있습니다. 예를 들어, Clojure의 자세한 문법과 함수들을 배우고 싶다면 [Clojure 공식 문서](https://clojure.org/guides/learn/syntax)를 참고할 수 있습니다. 또는 다른 라이브러리를 사용하여 더 복잡한 검색과 대체를 수행하고 싶다면, [Regex 정규식 라이브러리](https://github.com/clojure/clojure-contrib/blob/master/modules/regex/src/main/clojure/clojure/contrib/regex.clj)를 살펴볼 수 있습니다.