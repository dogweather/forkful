---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

문자열 추출은 문자열의 한 부분을 가져오는 것을 의미합니다. 이는 데이터를 분석하거나 처리할 때 필요한 특정 정보만을 추출하고 싶을 때 가장 많이 사용됩니다.

## 어떻게 사용하는가?

Clojure에서는 `subs` 함수를 사용하여 문자열에서 하위 문자열을 추출할 수 있습니다. 이는 첫 번째 인수로 대상 문자열을, 그리고 두 번째 및 세 번째 인수로 시작 및 끝 인덱스를 받습니다.

```clojure
(let [str "안녕하세요, Clojure!"]
  (println "원본 문자열:" str)
  (println "추출된 문자열:" (subs str 0 2)))
```

위 코드를 실행하면 아래와 같은 결과가 출력됩니다.

```
원본 문자열: 안녕하세요, Clojure!
추출된 문자열: 안녕
```

## 디프다이브

`subs` 함수는 Clojure의 문자열 처리 기능 중 하나로, Clojure 1.0부터 지원됩니다. 이 함수는 문자열의 시작 인덱스와 끝 인덱스를 기반으로 한 부분 문자열을 반환하며, 끝 인덱스가 주어지지 않았을 경우에는 시작 인덱스부터 문자열 끝까지의 모든 문자를 반환합니다.

추출하려는 범위가 문자열의 길이를 초과하는 경우 `subs` 함수는 IndexOutOfBoundsException을 발생시킵니다. 이러한 상황을 피하기 위해 `count` 함수를 사용하여 문자열의 길이를 미리 확인해야 합니다.

```clojure
(let [str "안녕하세요, Clojure!"]
  (println "원본 문자열:" str)
  (println "추출된 문자열:" (if (< (count str) 5)
                             (subs str 0 5)
                             (subs str 0 (count str)))))
```

## 참고 자료

[1] Clojure 공식 문서: `subs` 함수 - https://clojuredocs.org/clojure.core/subs

[2] Wikibook Clojure Programming: 문자열 처리 - https://en.wikibooks.org/wiki/Clojure_Programming/Strings

[3] Clojure for the Brave and True: 문자열과 패턴 - https://www.braveclojure.com/core-functions-in-depth/