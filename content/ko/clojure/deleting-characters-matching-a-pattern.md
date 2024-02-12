---
title:                "패턴에 일치하는 문자 삭제"
aliases:
- ko/clojure/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:07.102726-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 패턴과 일치하는 문자를 제거하는 것은 데이터 정제의 한 형태입니다. 필요 없는 데이터를 걸러내어 정보를 명확히 하고 프로그램의 효율성을 높이기 위해 사용됩니다.

## How to (실습 방법):
```Clojure
; 문자열에서 숫자 제거 예제
(defn remove-digits [s]
  (clojure.string/replace s #"\d+" ""))

; 샘플 문자열과 함수 실행
(def sample-str "ab1c3de45f")
(def cleaned-str (remove-digits sample-str))

; 출력 결과
println cleaned-str  ; "abcdef"
```

```Clojure
; 특정 문자 제거 예제
(defn remove-chars [s chars]
  (clojure.string/replace s (re-pattern (str "[" chars "]")) ""))

; 샘플 문자열과 함수 실행
(def sample-str2 "hello-world!")
(def cleaned-str2 (remove-chars sample-str2 "o-"))

; 출력 결과
println cleaned-str2  ; "hellwrld!"
```

## Deep Dive (심층 학습):
패턴 매칭으로 문자를 제거하는 기술은 문자열 처리에서 시작된 고전적인 기법으로, 정규 표현식(regular expressions)이 기반이 되었습니다. 클로저에서는 `clojure.string/replace` 함수와 정규 표현식을 사용하여 이를 간단하게 수행할 수 있습니다. 하스켈, 루비 등의 언어와 함께 클로저도 함수형 프로그래밍에서 문자열 처리에 필수적인 도구를 제공합니다.고성능 처리가 필요할 때는 Java의 `StringBuilder`를 사용하는 방법도 있습니다. 물론 다른 라이브러리를 사용하여 비슷한 일을 할 수도 있지만, 클로저의 표준 라이브러리만으로도 충분히 효과적입니다.

## See Also (관련 자료):
- 클로저 공식 문서: [clojure.string documentation](https://clojure.github.io/clojure/clojure.string-api.html)
- 자바스크립트와의 비교: [Mozilla RegExp Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- 정규 표현식 자세한 설명: [Regular-Expressions.info](https://www.regular-expressions.info/)
