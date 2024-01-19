---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식은 문자열에서 특정 패턴을 찾거나 대체하는 방법입니다. 프로그래머들이 이를 사용하는 이유는 복잡한 검색 및 대체 작업을 간단하게 수행할 수 있기 때문입니다.

## 어떻게:

Clojure에서 정규 표현식을 사용하는 데는 re-find, re-seq 등의 함수들이 있습니다.

```Clojure
(def str "Hello Clojure Programming!") 
(re-find #"\w+" str) ; -> "Hello"
(re-seq #"\w+" str) ; -> ("Hello" "Clojure" "Programming")
```
여기서 `#"\w+"`는 하나 이상의 연속된 문자나 숫자를 찾는 정규 표현식입니다.

## 깊은 탐구:

정규 표현식은 20세기 50년대 이래로 문자열 탐색과 패턴 매칭의 표준 도구로 사용되었습니다. Perl이나 Python같은 언어는 정규 표현식을 통합해서 사용합니다. Clojure도 이를 염두에 두고 디자인되었지만, 정규 표현식 대신에 전통적인 함수형 프로그래밍 접근 방식을 사용하여 문자열을 처리할 수 있는 방법들을 제공합니다.

정규 표현식의 대안으로는 Clojure의 `split`, `join`, `replace`같은 함수들이 있습니다. 이 함수들은 텍스트 처리를 위한 유연성을 제공하며, 때때로 정규 표현식보다 이해하기 쉽고 코드를 읽는 데 더 친숙할 수 있습니다.

## 더 보기:

- Clojure에서 정규 표현식을 더 잘 사용하는 방법은 [Clojure의 공식 문자열 API 문서](https://clojure.github.io/clojure/clojure.string-api.html)를 참고하세요.
- 정규 표현식에 대한 더 깊은 이해를 위해, [정규 표현식 입문서](https://www.regular-expressions.info/tutorial.html)가 유용할 수 있습니다.