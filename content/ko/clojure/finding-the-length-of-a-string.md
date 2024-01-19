---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열의 길이를 찾는 것은 특정 문자열이 얼마나 많은 문자를 포함하고 있는지를 측정하는 것입니다. 이는 검색, 정렬, 데이터 검증 등 다양한 프로그래밍 작업에서 필요로 합니다.

## 어떻게 하는가:

Clojure에서는 `count` 함수를 사용하여 문자열의 길이를 찾을 수 있습니다. 다음은 코드 예시입니다:

```Clojure
(defn string-length [s]
  (count s))

(println (string-length "안녕하세요"))
```

위 코드를 실행하면 출력 결과는 다음과 같습니다:

```Clojure
5
```

"안녕하세요"라는 문자열은 5개의 문자를 가지고 있으므로 출력 결과는 5입니다.

## 심화 학습

문자열의 길이를 찾는 방법은 프로그래밍 언어가 나오기 시작한 초기부터 사용되었습니다. 대부분의 언어에서는 이러한 기능을 내장 함수로 제공하는데, Clojure도 이 중 하나입니다.

변형으로 `reduce` 함수를 사용할 수도 있습니다. 기본 아이디어는 문자열을 순회하면서 카운터를 증가시키는 것입니다. 다음은 코드 예시입니다:

```Clojure
(defn string-length [s]
  (reduce (fn [acc _] (inc acc)) 0 s))

(println (string-length "안녕하세요"))
```

하지만 이 방법은 `count` 함수를 사용하는 것보다 복잡하고 시간이 더 많이 걸리므로 일반적으로 사용되지 않습니다.

## 참고 자료

다음은 문자열의 길이를 찾는 방법에 대한 추가 정보를 찾을 수 있는 몇 가지 링크입니다:

1. [Clojure 공식 문서](https://clojure.org/guides/learn/functions)
2. [Clojure 스트링 함수에 대한 SO 질문](https://stackoverflow.com/questions/2421653/how-to-get-the-length-of-string-in-clojure)
3. [Clojure의 리듀스 함수에 대한 설명](https://clojuredocs.org/clojure.core/reduce)