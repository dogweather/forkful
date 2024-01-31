---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)

표준 오류는 오류 메시지 나 프로그램의 진단 출력을 디스플레이하는 특수한 출력 스트림입니다. 프로그래머들은 오류와 로그를 표준 출력과 구분하기 위해 이를 사용합니다.

## How to (방법)

```clojure
;; 표준 오류로 메시지를 쓰는 예제
(. System err (println "이것은 표준 오류 메시지입니다."))

;; 기대되는 출력:
;; 이것은 표준 오류 메시지입니다.
```

## Deep Dive (심층 분석)

표준 오류는 UNIX 시스템에서 오랫동안 사용되었다. `System/err`는 자바의 표준 오류에 연결되어 있으며 클로저에서 바로 사용할 수 있다. 대안으로 로깅 라이브러리를 사용할 수 있지만 직접 표준 오류 스트림에 작성하는 것이 더 단순할 때가 있다. `println` 이외에 `print`, `printf` 함수도 사용할 수 있습니다.

## See Also (참고 자료)

- Clojure 공식 문서: [https://clojure.org/](https://clojure.org/)
- JavaDoc for `System/err`: [https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
