---
title:    "Clojure: 문자열 대문자로 변환하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

자바에서 string 값을 대문자로 변환하는 것은 프로그래머들에게 매우 중요합니다. 이 기능을 통해 문자열을 정렬하거나 비교하는 등 다양한 작업을 수행할 수 있습니다.

## 사용 방법

문자열을 대문자로 변환하는 방법은 매우 간단합니다. 먼저 `toUpperCase` 함수를 사용하여 문자열을 모두 대문자로 변환한 다음 출력합니다.

```Clojure
(def myString "hello world")
(println (toUpperCase myString))
```

`toUpperCase` 함수를 사용해 `myString` 변수를 대문자로 변환하면 "HELLO WORLD"가 출력됩니다.

## 깊이 파고들기

대문자로 변환하는 함수인 `toUpperCase`는 내부적으로 `Character/toUpperCase` 함수를 사용합니다. 이 함수는 유니코드 문자 값을 대문자로 변환해주는 역할을 합니다.

대부분의 인코딩은 ASCII 문자에만 대소문자 구분이 적용되지만 유니코드는 대소문자 구분이 더 다양하게 적용됩니다. 따라서 문자열을 비교하거나 정렬할 때 항상 대소문자를 일치시켜줘야 합니다.

## 관련 링크

- 유니코드와 대소문자 변환에 대한 더 자세한 정보는 [이 문서](https://unicode.org/faq/casemap_charprop.html)를 참조하세요.
- Clojure 공식 문서에서 [Character/toUpperCase 함수](https://clojuredocs.org/clojure.core/Character/toUpperCase)에 대한 정보를 확인할 수 있습니다.