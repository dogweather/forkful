---
title:                "문자열의 길이 찾기"
html_title:           "Clojure: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜:

문자열의 길이를 찾는 것에 참여하는 이유는 자주 사용되고, 많은 문제에서 필요한 기본적인 작업입니다.

## 어떻게:

```Clojure
(def str "Hello, world!")
(count str)

;; Output: 13
```

자주 사용되는 `count` 함수를 사용해서 문자열의 길이를 찾을 수 있습니다. 이 함수는 문자열의 모든 요소를 순회하고, 요소의 개수를 리턴합니다.

## 깊이 파헤치기:

사실, `count` 함수는 더 많은 일을 할 수 있습니다. 예를 들어, `vector`, `set`, `map` 등의 컬렉션뿐만 아니라, 문자열을 비롯한 모든 시퀀스의 길이도 찾을 수 있습니다. 또한, `count` 함수는 `seq` 함수와 함께 사용할 때, 시퀀스의 실제 길이를 구하는 것이 가능합니다. 

## 참고:

- [문자열 길이 찾기 함수 (Official Clojure Documentation)](https://clojuredocs.org/clojure.core/count)
- [Clojure 문자열 함수 (Modus Create)](https://moduscreate.com/blog/clojure-string-functions/)