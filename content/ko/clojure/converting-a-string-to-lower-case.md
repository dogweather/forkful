---
title:                "Clojure: 대소문자로 변환하기"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 작업은 프로그래밍에서 매우 일반적인 작업입니다. 이 작업은 대소문자를 구분하지 않는 검색, 비교, 정렬 등에서 유용하게 사용될 수 있습니다.

## 방법

Clojure에서는 `lower-case` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다.

```Clojure
(lower-case "HELLO WORLD")
```

이 코드를 실행하면 다음과 같은 결과가 나옵니다.

```Clojure
"hello world"
```

또는 `reduce` 함수를 사용하여 문자열의 각 문자를 소문자로 변환할 수도 있습니다.

```Clojure
(reduce str (map #(Character/toLowerCase %) "HELLO WORLD"))
```

이 코드를 실행하면 다음과 같은 결과가 나옵니다.

```Clojure
"hello world"
```

## 깊이 탐구

`lower-case` 함수는 주어진 문자열을 소문자로 변환한 새로운 문자열을 반환합니다. 따라서 원본 문자열은 수정되지 않습니다. 이는 성능 면에서 유리하지만, 원본 문자열을 수정하고 싶은 경우에는 `clojure.string` 네임스페이스의 `lower-case!` 함수를 사용할 수 있습니다.

또한 `reduce` 함수를 사용하여 문자열의 각 문자를 소문자로 변환하는 방법은 성능 면에서 조금 더 비효율적일 수 있습니다. 이를 해결하기 위해서는 `clojure.string` 네임스페이스의 `lower-case` 함수를 사용하는 것이 좋습니다.

## 참고 자료

[String functions in Clojure](https://clojuredocs.org/clojure.string/lower-case)

[Clojure String API](https://clojure.github.io/clojure/string-api.html)

## 참고자료

[Clojure에서 문자열을 소문자로 변환하는 방법 | 학원돌이의 블로그](https://blog.vanmoof.kr/entry/Clojure%EC%97%90%EC%84%9C-%EB%AC%B8%EC%9E%90%EC%97%90%EC%84%9C-%EC%86%8C%EB%AC%B8%EC%9E%90%EB%A1%9C-%EB%B3%80%ED%99%98%ED%95%98%EB%8A%94-%EB%B0%A9%EB%B2%95)

[Clojure에서 대소문자 변환하기 | Heesob의 블로그](https://blog.naver.com/hshsh567/221295662541)

[Clojure 문자열 다루기 | Nacyot의 프로그래밍 일기](http://blog.nacyot.com/articles/2015-02-12-clojure-string-pattern-match/)