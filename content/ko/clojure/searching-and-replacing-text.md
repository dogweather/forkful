---
title:                "Clojure: 텍스트 검색 및 교체"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 일은 프로그래밍에서 매우 중요합니다. 예를 들어, 여러분이 큰 데이터 세트에서 특정한 문자열을 찾고 그 문자열을 다른 것으로 대체하고 싶다고 가정해보세요. 이런 작업은 노력과 시간이 많이 들지만 올바른 도구를 사용하면 효율적으로 처리할 수 있습니다.

## 어떻게?

Clojure에서는 검색과 바꾸기를 쉽게 처리할 수 있는 `clojure.string/replace` 함수가 있습니다. 이 함수는 세 개의 인자를 받습니다. 첫 번째 인자는 원본 문자열입니다. 두 번째 인자는 검색할 대상 문자열이고 세 번째 인자는 바꿀 문자열입니다. 예를 들어, 만약 문자열 "hello"에서 "e"를 "a"로 바꾸고 싶다면 다음과 같이 작성할 수 있습니다.

```Clojure
(clojure.string/replace "hello" "e" "a")
```

출력 결과는 "hallo"가 됩니다. 그러나 이 함수는 첫 번째로 발견된 문자열만 바꿔주기 때문에 모든 "e"를 "a"로 바꾸려면 어떻게 해야 할까요?

## 깊이 파고들기

위의 예제에서는 우리가 대체할 문자열의 위치를 알고 있기 때문에 하드코딩할 수 있었습니다. 하지만 만약 대체할 문자열의 위치를 모를 경우는 어떨까요? 이때는 정규표현식(regular expressions)을 이용할 수 있습니다. 정규표현식은 빠르고 유연하게 문자열을 검색하고 바꾸는데 사용할 수 있습니다.

예를 들어, 만약 모든 "e"를 "a"로 바꾸고 싶을 때는 정규표현식 `#"\e"`를 사용할 수 있습니다. `#"\e"`는 "e"를 검색한다는 의미이고 `#"\a"`는 "a"로 대체한다는 의미입니다. 따라서 `clojure.string/replace` 함수를 다음과 같이 사용할 수 있습니다.

```Clojure
(clojure.string/replace "hello" #"\e" "a")
```

출력 결과는 여전히 "hallo"가 됩니다. 원하는 모든 "e"가 "a"로 대체되었습니다.

## 더 알아보기

Clojure에서는 정규표현식을 사용하지 않고도 문자열을 검색하고 바꿀 수 있는 다양한 함수와 라이브러리가 있습니다. 따라서 여러분이 좀 더 깊이 있는 정보를 알고 싶다면 Clojure 공식 문서를 참고하시기 바랍니다.

## 또 다른 정보

- [Clojure 공식 문서](https://clojure.org/)
- [정규표현식에 대한 쉬운 설명](https://regexone.com/)
- [Clojure 정규표현식 라이브러리](https://clojuredocs.org/clojure.string/replace)