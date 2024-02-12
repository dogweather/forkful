---
title:                "부분 문자열 추출"
aliases:
- /ko/clojure/extracting-substrings.md
date:                  2024-01-20T17:45:50.566280-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열을 추출하는 것은 문자열의 특정 부분을 선택하는 과정입니다. 프로그래머들은 데이터를 파싱하거나 포맷을 변환하고 특정 조건에 맞는 정보를 추출하기 위해 이 작업을 수행합니다.

## How to: (방법)
Clojure에서 부분 문자열을 추출할 때 주로 `subs` 함수를 사용합니다. 다음은 간단한 예제들입니다:

```clojure
;; 부분 문자열 추출하기
(subs "Hello, World!" 7) ; "World!"
(subs "안녕하세요, Clojure!" 7 14) ; "Clojure"

;; 문자열 길이
(count "Hello, World!") ; 13
(count "안녕하세요, Clojure!") ; 17
```

이 예제들은 각각 문자열에서 시작 위치 또는 시작 및 종료 위치를 지정해 추출하는 방법을 보여줍니다. `subs`는 시작 인덱스가 포함되고 종료 인덱스는 제외된 범위의 부분 문자열을 반환합니다.

## Deep Dive (심층 분석)
Clojure에서 `subs`는 문자열을 처리할 때 기본적인 도구입니다. 이는 자바 문자열의 `substring` 메서드에 기반을 두고 있으며, 불변성과 쓰레드 안전성을 보장하는 클로저의 설계 철학을 반영합니다. 문자열 처리에 다른 함수들, 예를 들어 `str`, `replace`, `join` 등도 사용되지만 목적에 따라 적절한 함수를 선택하는 것이 중요합니다. `subs`는 특히 인덱스를 직접 제어할 수 있어 문자열에서 특정 세그먼트를 추출할 때 유용합니다.

## See Also (더 보기)
더 다양한 문자열 처리에 대해서는 다음 출처들을 참고하세요:
- Clojure 문자열 API 공식 문서: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
- "Practical Clojure" 책에서의 문자열 처리 장
- 사례 연구: Clojure를 사용한 실제 문자열 처리 예제 검색

이 문서들은 clojure.string 네임스페이스에 포함된 다른 유용한 함수들을 파악하고, 문자열을 효율적으로 조작하는 데 도움을 줄 것입니다.
