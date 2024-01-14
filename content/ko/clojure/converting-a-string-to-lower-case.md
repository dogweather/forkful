---
title:                "Clojure: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 작업을 맡게 될 이유는 간단합니다. 프로그래밍에서는 일반적으로 문자열을 소문자로 사용하기 때문입니다. 예를 들어, 입력한 문자열과 비교할 때 대소문자를 구분하지 않고 비교하고 싶을 때, 소문자로 변환한 뒤 비교하면 더 쉽고 간편합니다.

## 하기

문자열을 소문자로 변환하는 방법은 매우 쉽습니다. 다음 코드 예제를 참고해주세요:

```Clojure
(defn to-lower [input-str]
  (clojure.string/lower-case input-str))

(to-lower "HELLO WORLD!") ; 출력 결과: "hello world!"
```

위의 코드 예제에서는 `defn` 키워드를 사용하여 `to-lower`라는 함수를 정의하고, `clojure.string/lower-case` 함수를 사용하여 입력된 문자열을 소문자로 변환하였습니다. 이렇게 간단하게 문자열을 소문자로 변환할 수 있습니다.

## 깊게 살펴보기

문자열을 소문자로 변환하는 작업은 `to-lower` 함수를 사용하여 간단하게 수행할 수 있지만, 실제로는 더 많은 작업을 수행하게 됩니다. 예를 들어, 한글 문자열은 ASCII 문자와 다른 문자셋을 사용하기 때문에, 영어와 한글을 모두 소문자로 변환하기 위해서는 추가적인 작업이 필요합니다. 또한, Unicode나 UTF-8 문자열을 소문자로 변환할 때도 주의해야 합니다. 따라서, 프로젝트에 따라 적합한 문자열 변환 함수를 선택하는 것이 중요합니다.

## 참고

- [Clojure 공식 문서 - 문자열 변환 함수](https://clojure.org/reference/strings#_lowercase_normalization)
- [Clojure Cookbook - 문자열을 소문자로 변환하기](https://clojure-cookbook.com/strings/capitalizers)
- [Stack Overflow - Clojure에서 문자열 대소문자 변환하기](https://stackoverflow.com/questions/737293/uncommon-clojure-convert-string-to-lower-upper-camel-case)