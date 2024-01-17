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

## 무엇 & 왜?

문자열의 길이를 찾는 것은 매우 유용합니다. 이를 통해 우리는 어떤 문자열이 몇 개의 문자로 이루어져 있는지 알 수 있습니다. 프로그래머들은 이를 통해 사용자가 입력한 값의 길이를 확인하거나, 특정 문자열이 조건에 부합하는지를 검사할 때 자주 사용합니다.

## 방법:

```Clojure
(count "Hello World")
;; Output: 11

(count "안녕하세요")
;; Output: 5 
```

## 깊게 들어가보기:

문자열의 길이를 구하는 방법은 간단합니다. Clojure에서는 ```count``` 함수를 사용하여 문자열에 포함된 문자의 개수를 반환합니다. 이 함수는 Clojure의 기본 함수이기 때문에 별도의 라이브러리 설치가 필요하지 않습니다. 다른 언어들에서는 보통 문자열의 길이를 구하는 함수가 따로 있지만, Clojure에서는 컬렉션에 포함된 요소들의 개수를 구할 때 사용할 수 있도록 일반화된 함수를 제공합니다. 또한, 문자열의 길이를 구하는 대신 인덱스의 길이를 구할 수도 있습니다. 문자열의 경우, 첫번째 글자의 인덱스는 0이기 때문에 count 함수의 결과값에 1을 더해주면 인덱스의 길이를 구할 수 있습니다.

## 더 알아보기:

더 자세한 정보를 원한다면, [Clojure docs](https://clojuredocs.org/clojure.core/count)를 참고해보세요. 또한, 다른 언어의 문자열 길이 구하는 방법과 비교해보면 흥미로울 수 있으니 참고해보세요.

## 관련 자료:

- [Clojure docs](https://clojuredocs.org/clojure.core/count)
- [Stack Overflow - How to get the length of a string in Clojure](https://stackoverflow.com/questions/20461895/how-to-get-the-length-of-a-string-in-clojure)