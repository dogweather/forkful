---
title:    "Clojure: 문자열의 길이 찾기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 대해 관심이 있는 이유는 단순히 프로그래밍 세계에서 가장 기본적인 작업 중 하나이기 때문입니다. 프로그래머라면 모두가 문자열의 길이를 알아야 하는 상황에 직면할 수 있기 때문에 이를 이해하고 활용하는 것은 매우 중요합니다.

## 어떻게

```
(defn string-length [str]
  (count str))

(def my-string "안녕하세요")
(string-length my-string)
```

위의 코드는 Clojure로 문자열의 길이를 찾는 방법을 보여줍니다. ```string-length``` 함수는 문자열의 길이를 알려주는 내장 함수인 ```count```를 사용하여 문자열의 길이를 반환합니다. 이렇게 간단한 방법으로 문자열의 길이를 찾을 수 있습니다.

## 깊게 파고들기

다른 프로그래밍 언어와 비교하면 Clojure에서는 문자열의 길이를 찾는 것이 매우 간단합니다. 그 이유는 Clojure가 Immutable한 언어이기 때문입니다. 즉, 문자열은 변경 불가능하기 때문에 문자열의 길이를 저장하거나 수정할 필요가 없기 때문입니다.

또한 Clojure는 다른 언어에 비해 효율적인 문자열 처리를 제공합니다. 예를 들어, ```count``` 함수는 문자열의 길이를 구하기 위해 문자열 전체를 반복하지 않고 내부적으로 문자열의 길이를 저장하고 있기 때문에 실행 시간이 절약됩니다.

## 계속 찾아보기

- [Clojure 공식문서](https://clojure.org/reference/strings)
- [Repl.it에서 Clojure 코딩해보기](https://repl.it/languages/clojure)
- [Clojure 커뮤니티 포럼](https://ask.clojure.org/)