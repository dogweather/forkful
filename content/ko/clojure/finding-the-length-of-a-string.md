---
title:                "Clojure: 문자열의 길이 찾기"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜 이것을 해야 할까?

주어진 문자열의 길이를 찾는 것은 프로그래머에게 중요한 개념입니다. 문자열은 다양한 작업에 필수적으로 사용되기 때문에, 문자열의 길이를 알아내는 것은 프로그램을 작성할 때 매우 유용합니다.

## 어떻게 하나요?

우선, 문자열의 길이를 찾는 가장 기본적인 방법은 `count` 함수를 사용하는 것입니다. 이 함수는 주어진 문자열의 길이를 정수로 반환합니다.

```Clojure
(count "Hello") ; 출력: 5
```

또 다른 방법은 `seq` 함수와 `count` 함수를 함께 사용하는 것입니다. `seq` 함수는 주어진 문자열을 문자의 시퀀스(sequence)로 변환하여 `count` 함수를 적용합니다.

```Clojure
(count (seq "Hello")) ; 출력: 5
```

`seq` 함수를 사용하는 방법은 문자열 뿐만 아니라 다른 시퀀스 데이터에 대해서도 적용할 수 있기 때문에, 더 유연한 방법입니다.

## 깊이 파헤쳐보기

`count` 함수가 어떻게 동작하는지 더 자세히 알아보겠습니다. `count` 함수는 사실 `coll` 프로토콜의 `count` 메서드를 호출하는 것입니다. 이 메서드는 주어진 컬렉션의 길이를 계산하는 데 사용됩니다.

문자열은 사실 상수(Constant)와 같은 컬렉션이며, `count` 메서드는 문자열의 길이를 반환하는 것입니다. 이러한 이유로 `count` 함수는 문자열에 대해서도 동작합니다.

## 관련 자료

- [ClojureDocs: count](https://clojuredocs.org/clojure.core/count)
- [ClojureDocs: seq](https://clojuredocs.org/clojure.core/seq)
- [ClojureDocs: coll protocol](https://clojuredocs.org/clojure.core.protocols/coll)
- [블로그: Clojure 강좌 - 문자열의 길이 찾기](https://blog.naver.com/clojure/1234567890)