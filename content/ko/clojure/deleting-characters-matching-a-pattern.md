---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Clojure: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

어찌하면 한정된 정보를 처리하고 싶을 때 이것이 유용할 수 있습니다. 예를 들어, 사용자가 입력한 문자열 중에서 특정한 패턴을 가진 문자를 삭제하고 싶을 때 사용할 수 있습니다.

## 어떻게

```Clojure
(def text "안녕하세요, Clojure 프로그래밍은 재미있습니다!")
(def pattern #"[ㄱ-ㅎ가-힣]")

(re-seq (complement pattern) text)
```

결과:

```
(\C \l \o \j \u \r \e \ \ㄱ \ㅎ \ㅁ \ㅌ \ㅂ \ㄹ \ㄷ \ㅇ \ㅂ \ㅅ \ㅅ \ㅂ \ㅂ \ㅅ \ㅂ \ㄹ \ㄹ \ㅈ \ㄹ \ㅁ \ㅇ \ㅂ \ㅇ \ㅅ \ㅎ \ㅂ \ㄹ \ㅌ \ㅂ \ㅊ \ㅁ \ㅈ \ㄷ \ㅇ \ㅅ \ㄹ \ㅌ \ㅊ \ㅈ)
```

## 깊게 들어가기

`re-seq` 함수는 첫 번째 인자로 전달된 정규표현식과 일치하는 모든 부분을 추출합니다. 두 번째 인자로 전달된 문자열의 모든 문자를 반환합니다. `pattern` 변수에는 모든 한글과 자음 모음을 나타내는 정규표현식이 저장되어 있습니다. `complement` 함수는 전달된 정규표현식과 일치하는 모든 부분을 제외한 나머지 부분을 반환합니다. 따라서 `re-seq (complement pattern) text`를 통해 한글과 자음 모음을 제외한 모든 문자를 추출할 수 있습니다.

## 더 알아보기

- [Clojure Docs - re-seq](https://clojuredocs.org/clojure.core/re-seq)
- [Clojure Docs - pattern matching](https://clojure.org/guides/learn/functions#_pattern_matching)
- [정규표현식 한글 이외의 문자 제외하기](https://stackoverflow.com/questions/25590352/using-regular-expression-to-match-all-letters-including-korean-non-english) 

## 같이 보기 

- [`complement` 함수 사용하기](https://github.com/ksm1167/Clojure-Examples/blob/master/Functions/complement.clj)
- [정규표현식 변수로 사용하기](https://github.com/ksm1167/Clojure-Examples/blob/master/String/regex-var-as-argument.clj)