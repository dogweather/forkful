---
title:                "문자열 대문자 변환"
html_title:           "Clojure: 문자열 대문자 변환"
simple_title:         "문자열 대문자 변환"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜
왜 누군가 문자열을 대문자로 변환하는 것에 참여할까요? 대문자로 작성된 문자열은 주로 사용자 입력을 정리하거나 데이터 처리를 위해 사용됩니다.

# 어떻게
Clojure에서는 문자열을 대문자로 변환하기 위해 `clojure.string/capitalize` 함수를 사용합니다. 아래는 간단한 예시 코드입니다.

```Clojure
(require '[clojure.string :as str])

(str/capitalize "hello") ; => "Hello"
(str/capitalize "goodbye") ; => "Goodbye"
```

여러 단어가 있는 문자열의 경우, 첫 번째 단어만 대문자로 변환하고 나머지는 소문자로 유지됩니다.

```Clojure
(str/capitalize "hello world") ; => "Hello world"
(str/capitalize "goodbye cruel WORLD") ; => "Goodbye cruel world"
```

# 심층 분석
`clojure.string/capitalize` 함수는 내부적으로 `java.lang.Character/toUpperCase` 함수를 사용하여 문자열을 대문자로 변환합니다. 이 함수의 경우 Unicode 문자를 처리할 수 있으므로 다국어 환경에서도 잘 작동합니다.

# 참고
- [Clojure Docs: clojure.string/capitalize](https://clojuredocs.org/clojure.string/capitalize)
- [Java Docs: java.lang.Character/toUpperCase](https://docs.oracle.com/javase/10/docs/api/java/lang/Character.html#toUpperCase(int))