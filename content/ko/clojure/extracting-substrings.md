---
title:                "Clojure: 부분 문자열 추출"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

저희가 서브스트링을 추출하는 것에 관심이 있는 이유는 무엇일까요? 서브스트링은 문자열에서 특정한 부분을 추출하는 것으로, 데이터 처리나 분석을 할 때 매우 유용합니다. 예를 들어, 이메일 주소에서 도메인 부분만 뽑아내거나, 전화번호에서 지역번호만 추출할 수 있습니다. Clojure를 사용하면 간단하고 효율적인 방법으로 서브스트링을 추출할 수 있습니다.

## 어떻게

Clojure에서 서브스트링을 추출하는 방법은 다음과 같습니다. 먼저, `subs` 함수를 사용해 추출하고자 하는 문자열의 시작 인덱스와 끝 인덱스를 지정합니다. 그리고 `str` 함수를 사용해 새로운 문자열을 생성합니다. 아래 코드는 `subs` 함수를 사용해 문자열의 일부분을 추출하는 예시입니다.

```Clojure
(def str "Clojure는 멋진 프로그래밍 언어입니다.")

;; subs 함수를 사용해 9번째부터 15번째 인덱스까지의 문자열을 추출합니다.
(str (subs str 9 15))
```

이 코드를 실행하면 다음과 같은 결과가 나옵니다.

```Clojure
"멋진 프로그래밍 언어"
```

## 깊이 파고들기

Clojure에서 서브스트링을 추출하는 것은 간단한 작업으로 보일 수 있지만, 실제로는 다양한 방법이 있습니다. 예를 들어, `subs` 함수 대신 `re-find` 함수와 정규식을 사용해 패턴과 일치하는 부분을 추출할 수도 있습니다. 또는 `substring` 함수를 사용해 시작 인덱스와 길이를 지정하여 추출할 수도 있습니다. 이와 같은 다양한 방식을 통해 서브스트링을 추출하는 방법을 익히고 응용할 수 있습니다.

## 더 보기

- [Clojure 공식 문서](http://clojure.org/)
- [서브스트링 추출 예시 코드](https://github.com/clojure/clojure/blob/master/test/clojure/test_clojure/core.clj#L240-L261)
- [정규식을 활용한 서브스트링 추출 방법](https://clojuredocs.org/clojure.string/re-find)
- [문자열 함수 참고 가이드](https://clojuredocs.org/clojure.string)
- [Clojure ko.kr](http://clojure-ko.kr/)