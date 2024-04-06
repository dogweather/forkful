---
date: 2024-01-20 17:42:07.102726-07:00
description: "How to (\uC2E4\uC2B5 \uBC29\uBC95): \uD328\uD134 \uB9E4\uCE6D\uC73C\uB85C\
  \ \uBB38\uC790\uB97C \uC81C\uAC70\uD558\uB294 \uAE30\uC220\uC740 \uBB38\uC790\uC5F4\
  \ \uCC98\uB9AC\uC5D0\uC11C \uC2DC\uC791\uB41C \uACE0\uC804\uC801\uC778 \uAE30\uBC95\
  \uC73C\uB85C, \uC815\uADDC \uD45C\uD604\uC2DD(regular expressions)\uC774 \uAE30\uBC18\
  \uC774 \uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uD074\uB85C\uC800\uC5D0\uC11C\uB294 `clojure.string/replace`\
  \ \uD568\uC218\uC640 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uC774\uB97C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.486984-06:00'
model: gpt-4-1106-preview
summary: "\uD328\uD134 \uB9E4\uCE6D\uC73C\uB85C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\
  \uB294 \uAE30\uC220\uC740 \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC5D0\uC11C \uC2DC\uC791\
  \uB41C \uACE0\uC804\uC801\uC778 \uAE30\uBC95\uC73C\uB85C, \uC815\uADDC \uD45C\uD604\
  \uC2DD(regular expressions)\uC774 \uAE30\uBC18\uC774 \uB418\uC5C8\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

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
