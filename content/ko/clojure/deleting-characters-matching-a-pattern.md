---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

특정 패턴에 일치하는 문자를 삭제하는 것이란 코드 내에서 특정 패턴의 문자들을 탐색하고 이들을 제거하는 프로그래밍 기법을 의미합니다. 이런 작업은 불필요한 공백, 특수 문자 등을 없애거나 데이터를 정리하기 위해 주로 사용됩니다.

## 이렇게 하세요:

Clojure에서 문자열에서 특정 문자를 제거하는 방법을 살펴보겠습니다. 여기에는 `clojure.string/replace` 함수를 사용하여 선택한 문자를 없애는 방법이 포함됩니다.

```clojure
(require '[clojure.string :as str])

(defn delete-chars [s chars]
  (str/replace s #"(?i)[chars]" ""))

(println (delete-chars "Hello, World!" "o"))
```
위 코드는 문자열 "Hello, World!"에서 모든 'o'를 제거합니다.

샘플 결과물은 다음과 같습니다:
```clojure
"Hell, Wrld!"
```
## 심층 분석:

문자열에서 패턴에 일치하는 문자를 제거하는 작업은 옛 컴퓨터 시스템에서 데이터를 정리하거나 파싱하는데 첫 번째로 사용되었습니다. `clojure.string/replace`는 이 문제에 대한 Clojure의 해결책입니다. 이것은 자바의 `java.lang.String` 메서드를 사용하여 구현되었습니다. 

대안으로는 'reduce'와 결합한 'filter'를 사용할 수 있습니다. 이 방법은 좀 더 복잡하지만, 큰 문자열을 처리하는 데 효율적일 수 있습니다.

```clojure
(defn delete-chars [s chars]
  (apply str (filter #(not (contains? chars %)) s)))
```

## 또 보세요:

문자열 조작 및 Clojure 프로그래밍에 대한 추가 정보는 다음의 링크를 참조하세요:

1. [Clojure.org: String Functions](https://clojure.org/guides/learn/functions#_string_functions)
2. [Clojure String API Overview](https://clojuredocs.org/clojure.string)
3. [StackOverFlow: How to remove certain characters from a string using Clojure](https://stackoverflow.com/questions/1920399/string-remove-substrings)