---
title:    "Clojure: 부분 문자열 추출하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

서브스트링을 추출하는 것에 대해 관심이 생길 수 있는 이유는 여러 가지가 있습니다. 예를 들어, 특정 문자열에서 원하는 부분만을 추출해서 사용할 수 있기 때문에 프로그래밍에서 매우 유용합니다. 또는 정보를 특정 패턴으로 분석하고 싶을 때, 서브스트링을 추출해서 필요한 부분만을 골라낼 수 있습니다.

## 어떻게

서브스트링을 추출하는 것은 Clojure에서 매우 쉽습니다. 다음과 같은 코드를 입력하면 됩니다.

```Clojure
(def str "안녕하세요. 저는 Clojure를 공부 중입니다.")

;; 첫번째 문자부터 세번째 문자까지 추출
(substring str 0 3)

;; 세번째 문자부터 마지막 문자까지 추출
(substring str 2)

;; 마지막 문자부터 두번째 문자까지 추출
(substring str -1 -3)
```

**출력:**

```
안녕
녕하세요. 저는 Clojure를 공부 중입니다.
 (; _`)
```

위 코드에서 사용된 `substring` 함수는 두 개 이상의 문자열을 다룰 때 매우 유용합니다. 예를 들어, 주어진 문자열에서 특정 단어만을 추출하고 싶을 때 사용할 수 있습니다.

```Clojure
(def str "Clojure는 함수형 프로그래밍 언어입니다.")

;; "Clojure"만을 추출
(substring str 0 7)
```

**출력:**

```
Clojure
```

## 심층 분석

Clojure에서의 서브스트링 추출은 `substring` 함수를 이용하는 것 외에도 다양한 방법이 있습니다. 예를 들어, `clojure.string` 라이브러리에서 제공하는 `join` 함수를 사용할 수 있습니다. 이 함수는 여러 문자열을 하나의 문자열로 합쳐줍니다. 이때 요소들은 `:separator` 옵션으로 구분할 수 있습니다.

또한, `substring` 함수는 문자열의 인덱스를 사용해서 추출하는 반면, `clojure.string` 라이브러리에서는 정규식을 사용해서 추출할 수 있는 `split` 함수도 제공됩니다.

더 자세한 내용은 [Clojure 공식 문서](https://clojuredocs.org/clojure.string/split)를 참고하세요.

## 또 참고해주세요.

- [Clojure 문자열 처리 관련 함수](https://clojuredocs.org/clojure.string)
- [Clojure 정규표현식 관련 라이브러리](https://github.com/halgari/clojure-regular-expressions)