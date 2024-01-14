---
title:                "Clojure: 문자열 연결"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 이어 붙이는 것은 프로그래밍에서 자주 사용되는 기술 중 하나입니다. 그것은 여러 문자열을 하나의 문자열로 합치는 과정을 말합니다. 이를 통해 코드의 가독성을 향상시키고, 문자열 처리를 간단하고 효율적으로 할 수 있습니다. 또한 다른 데이터 타입과 함께 사용하여 많은 일반적인 프로그래밍 문제를 해결할 수 있습니다.

## 어떻게

Clojure에서 문자열을 이어붙이는 가장 간단한 방법은 `str` 함수를 사용하는 것입니다. 이 함수는 하나 이상의 인자를 받아들이고, 각 인자를 하나의 문자열로 이어붙여 반환합니다. 예를 들면:

```Clojure
(str "안녕하세요, " "저는 " "Clojure를 " "사용합니다.")
```

이 코드는 다음과 같은 문자열을 반환합니다:

```Clojure
"안녕하세요, 저는 Clojure를 사용합니다."
```

또한 `str` 함수를 통해 숫자나 불리언 값을 문자열로 변환할 수도 있습니다. 예를 들어:

```Clojure
(str "나는" 25 "살입니다.")
```

위 코드는 다음과 같은 문자열을 반환합니다:

```Clojure
"나는 25살입니다."
```

## 심층 탐구

Clojure의 문자열 연결 기능은 내부적으로 자바의`StringBuffer` 클래스를 사용합니다. 이 클래스는 문자열을 변경 가능한 버퍼에 저장하여 성능을 향상시킵니다. 그러므로 `str` 함수를 반복해서 사용하는 것은 성능 저하를 일으킬 수 있습니다. 대신에 `clojure.string/join` 함수를 사용하여 여러 문자열을 효율적으로 이어붙일 수 있습니다. 예를 들어:

```Clojure
(require '[clojure.string :as str])

(str/join "-" ["사과" "배" "바나나"])
```

위 코드는 다음과 같은 문자열을 반환합니다:

```Clojure
"사과-배-바나나"
```

또한 `str` 함수를 사용하는 대신 Clojure의 템플릿 기능 중 하나인 `format` 함수를 사용하여 문자열을 이어붙일 수도 있습니다. 이 함수는 문자열 내에 `%` 기호를 삽입하여, 해당 위치에 인자들을 순서대로 삽입하는 방식으로 문자열을 형식화합니다. 예를 들어:

```Clojure
(format "나는 %s살이고, %s를 좋아합니다." 25 "Clojure")
```

위 코드는 다음과 같은 문자열을 반환합니다:

```Clojure
"나는 25살이고, Clojure를 좋아합니다."
```

## 관련 자료

- [Clojure 공식 문서](https://clojure.org/guides/learn/syntax#_strings)
- [다른 데이터 타입과 문자열 이어붙이기](https://www.tutorialspoint.com/clojure/clojure_data_types.htm)
- [`str` vs `format` 성능 비교](https://stackoverflow.com/questions/6666941/clojure-str-vs-format-performance)