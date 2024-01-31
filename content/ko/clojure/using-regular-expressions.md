---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why (무엇인가요? 그리고 왜죠?)
정규 표현식은 문자열을 처리할 때 필요한 패턴을 찾거나 대체하는데 씁니다. 프로그래머들은 데이터 검증, 검색, 변환 작업 등을 간편하게 처리하기 위해서 정규 표현식을 사용합니다.

## How to (어떻게 사용하나요?)
```Clojure
;; 문자열에서 숫자 찾기
(re-find #"\d+" "주문번호는 12345입니다.")
;; 출력: "12345"

;; 문자열에서 모든 숫자 찾기
(re-seq #"\d+" "주문번호 12345, 추적번호 67890")
;; 출력: ("12345" "67890")

;; 문자열 치환
(clojure.string/replace "가격: 10000원" #"\d+" "9999")
;; 출력: "가격: 9999원"
```

## Deep Dive (깊이 있게 살펴보기)
정규 표현식은 1950년대 초에 수학자 스티븐 클리니가 시작했고, 컴퓨터 과학에서 문자열 처리 알고리즘에 광범위하게 적용되었습니다. `regex`라이브러리나 자바의 `java.util.regex`같은 라이브러리를 대안으로 쓸 수도 있지만, Clojure에서는 자바 플랫폼을 바탕으로 하여 `re-find`, `re-seq` 같은 편리한 함수들을 내장하고 있습니다. 이 함수들은 JVM(Java Virtual Machine) 위에서 동작하며 자바의 정규 표현식 기능을 활용합니다.

## See Also (더 보기)
- Clojure 공식 문서: [https://clojure.org/](https://clojure.org/)
- Java 정규 표현식 문서: [https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
