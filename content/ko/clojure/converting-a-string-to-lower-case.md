---
title:                "문자열을 소문자로 변환하기"
html_title:           "Clojure: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
문자열을 소문자로 변환하는 것은 프로그래머들이 자주 수행하는 작업이며, 문자열을 비교하거나 검색할 때 용이하기 때문에 중요합니다.

# 어떻게:

```Clojure
(.toLowerCase "HELLO WORLD") ; => "hello world"
(.toLowerCase "I lOvE cLoJuRe") ; => "i love clojure"
```

# 깊이 파고들기:
1. 문자열을 소문자로 변환하는 기능은 오래전부터 프로그래밍 언어에서 지원되고 있었습니다.
2. Clojure에서는 `(.toLowerCase str)` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다.
3. 다른 대안으로는 `clojure.string/lower-case` 함수가 있으며, `(.toLowerCase)` 함수와 다른 점은 입력 값이 null일 때 예외를 발생시킨다는 것입니다.

# 관련 자료:
- Clojure 함수 레퍼런스: https://clojuredocs.org/clojure.core/lower-case
- Java 문자열 함수 레퍼런스: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()