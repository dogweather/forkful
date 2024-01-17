---
title:                "정규식 사용하기"
html_title:           "Clojure: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
정규 표현식을 사용하는 것은 매우 유용합니다. 프로그래머들은 대용량의 텍스트를 간편하고 효율적으로 처리할 수 있어서 정규 표현식을 사용합니다.

## 방법:
아래에는 반복되는 작업을 수행하기 위해 정규 표현식을 사용하는 코드 예제와 샘플 출력이 있습니다.

```Clojure 
; "re-matches" 는 정규 표현식을 사용하여 문자열에서 패턴을 찾아냅니다.
(re-matches #"ab*" "ababab") ; 결과: "ababab"

; "re-seq" 는 정규 표현식을 사용하여 문자열에서 패턴을 찾아 리스트 형태로 반환합니다.
(re-seq #"a+" "aaabbbb") ; 결과: ("aaa")

; "re-find" 는 정규 표현식을 사용하여 첫 번째로 일치하는 패턴을 찾아 리턴합니다.
(re-find #"c+d*" "cccddd") ; 결과: "ccc"

; "re-matches?" 는 문자열이 정규 표현식의 패턴과 일치하는지 여부를 판별합니다.
(re-matches? #"a*b" "aaaab") ; 결과: true
(re-matches? #"b+" "abab") ; 결과: false
```

## 심층 분석:
정규 표현식은 1950년대에 고안된 형식 언어로써, 텍스트를 처리하고 검색하는데 사용됩니다. 다른 언어에서도 정규 표현식을 사용할 수 있지만, Clojure는 정규 표현식을 이용하여 매우 간결하고 효율적인 코드를 작성할 수 있도록 도와줍니다. 또한 Purely Functional Programming Language라는 특징 상, 정규 표현식을 사용해도 부작용이 발생하지 않습니다.

## 더 보기:
- [Clojure 정규 표현식 공식 문서](https://clojuredocs.org/clojure.core/re-matches)
- [정규 표현식 간단 설명서](https://regexone.com/)
- [정규 표현식 연습 사이트](https://regex10.com/)