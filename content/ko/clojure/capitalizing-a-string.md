---
title:                "Clojure: 문자열을 대문자로 바꾸기"
simple_title:         "문자열을 대문자로 바꾸기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

본격적으로 프로그래밍을 시작하기 전에는 언어의 기본 개념들을 익혀야 합니다. 그 중 하나인, 문자열 대문자로 변환에 대해 알아보도록 하겠습니다.

## 왜

문자열을 대문자로 변환하는 이유는 다양합니다. 예를 들어, 데이터베이스에서 사용자 이름을 저장할 때 대부분 대문자로 저장하는 경우가 많습니다. 그렇기 때문에 그 데이터를 사용하여 다른 프로그램에서 사용할 때도 대문자로 일치시켜주는 것이 필요합니다. 또는 사용자들이 입력한 텍스트를 모두 대문자로 변환하여 처리하는 역할을 할 때도 있습니다.

## 어떻게 하나요

Clojure에서 문자열을 대문자로 변환하기 위해서는 `s>ASCII` 함수를 사용하면 됩니다. 아래 예제를 참고해주세요.

```Clojure
(s>ASCII "hello world")
;; 출력: "HELLO WORLD"
```

만약 대문자 뿐만 아니라 모든 글자를 소문자로 변환하고 싶다면 `s>lower-case` 함수를 사용하면 됩니다. 아래 예제를 참고해주세요.

```Clojure
(s>lower-case "Hello World")
;; 출력: "hello world"
```

Clojure에서는 다양한 함수를 사용하여 문자열을 변환할 수 있습니다. 또한 간단한 코드로도 쉽게 변환이 가능하므로 참고하시기 바랍니다.

## 깊게 파보기

Clojure에서 문자열을 대문자로 변환하는 메커니즘은 실제로는 문자열을 `seq` 함수를 사용하여 리스트 형식으로 변환한 뒤, 각각의 문자를 대문자로 바꿔주는 방식으로 이루어집니다. 이것을 통해 보다 자세히 문자열 변환의 원리를 이해할 수 있습니다.

## 관련 링크

- 문자열 함수 더 알아보기: https://clojure.org/reference/strings
- Clojure 입문: https://clojure.or.kr/guides/learn.html
- Clojure 한국어 문서: https://clojure.or.kr/