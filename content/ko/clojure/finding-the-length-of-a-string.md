---
title:    "Clojure: 문자열 길이 찾기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 대해 관심을 가지는 이유는 매우 간단합니다. 문자열의 길이를 알면 원하는 작업 뒤에 실행될 코드를 더 효율적으로 작성할 수 있기 때문입니다.

## 방법

Clojure에서 문자열의 길이를 찾는 방법은 간단합니다. `count` 함수를 사용하면 됩니다. 다음은 `count` 함수를 사용하는 예시 코드와 그에 따른 출력입니다.

```Clojure
(count "안녕하세요?")
```
> 7

```Clojure
(count "Hello, world!")
```
> 13

이렇게 간단한 코드로 문자열의 길이를 찾을 수 있습니다.

## 깊게 들어가기

Clojure에서 `count` 함수는 문자열 이외의 다른 데이터 유형도 모두 처리할 수 있습니다. 리스트, 벡터, 맵 등 다양한 유형의 데이터도 모두 길이를 찾을 수 있습니다. 그리고 이 함수는 속도도 빠르며 메모리 사용도 효율적입니다. 이러한 이유로 Clojure에서는 `count` 함수가 자주 사용되는 편입니다.

## 연관 자료

[공식 Clojure 문서 - count](https://clojuredocs.org/clojure.core/count)