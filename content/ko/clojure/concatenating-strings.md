---
title:                "Clojure: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결을 수행하는 이유는 다른 데이터를 조합하여 원하는 형식의 결과를 생성하기 위해서입니다.

## 어떻게

Clojure에서 문자열 연결은 `str` 함수를 사용하여 수행할 수 있습니다. 이 함수는 여러 개의 인자를 받을 수 있으며, 이를 모두 연결하여 하나의 문자열로 반환합니다. 예를 들어:

```Clojure
(str "안녕하세요" "저는" "Clojure를" "사랑합니다")
```

위 코드는 다음과 같은 문자열을 반환할 것입니다:

```
안녕하세요저는Clojure를사랑합니다
```

만약 `str` 함수에 숫자나 불리언 값 등의 다른 데이터 형식을 전달하면, 이는 모두 문자열로 변환되어 연결됩니다. 예를 들어:

```Clojure
(str "이름:" "Jane" ", 나이:" 25 ", 성별:" "여성")
```

위 코드는 다음과 같은 문자열을 반환할 것입니다:

```
이름: Jane, 나이: 25, 성별: 여성
```

또 다른 방법으로는 `+` 함수를 사용하는 것입니다. 이 함수는 숫자나 문자열을 모두 처리할 수 있으며, 인자로 받는 모든 값들을 더해서 하나의 문자열로 반환합니다. 예를 들어:

```Clojure
(+ "Clojure는" "매우" "좋은" "언어입니다.")
```

위 코드는 다음과 같은 문자열을 반환할 것입니다:

```
Clojure는매우좋은언어입니다.
```

## 깊게 들어가기

Clojure에서는 문자열 연결을 위해 `str` 함수 외에도 `clojure.string` 라이브러리에서 `join` 함수를 사용할 수 있습니다. 이 함수는 컬렉션을 받아서 지정된 구분자를 이용하여 모든 요소를 연결해서 문자열로 반환합니다. 예를 들어:

```Clojure
(require '[clojure.string :as str])
(str/join " " ["나는" "매일" "Clojure를" "공부합니다."])
```

위의 코드는 다음과 같은 문자열을 반환할 것입니다:

```
나는 매일 Clojure를 공부합니다.
```

`clojure.string` 라이브러리에는 `split` 함수도 있으며, 이 함수는 지정된 구분자를 기준으로 문자열을 쪼개서 컬렉션으로 반환합니다. 예를 들어:

```Clojure
(require '[clojure.string :as str])
(str/split "기사|정치|경제|스포츠|문화" #"\|")
```

위의 코드는 다음과 같은 컬렉션을 반환할 것입니다:

```
["기사" "정치" "경제" "스포츠" "문화"]
```

## 여기서 더 읽어보기

- [ClojureDocs: str 함수](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs: + 함수](https://clojuredocs.org/clojure.core/%2B)
- [The Clojure string API](https://clojuredocs.org/clojure.string)