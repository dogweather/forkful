---
title:                "문자열 연결"
html_title:           "Clojure: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

스트링을 합치는 것이 왜 중요한지 궁금하신가요? 바로 프로그래밍에서 문자열을 처리하는 작업이 많기 때문입니다. 예를 들어, 데이터베이스에 저장된 정보를 HTML 페이지로 표현하기 위해서는 문자열을 결합하여 필요한 형식으로 바꾸는 작업이 필요합니다. 또한 프로그램을 실행하는 도중에 다양한 메시지를 출력하기 위해서도 문자열 합치기가 필요합니다.

## 어떻게?

Clojure에서는 "+" 연산자를 사용하여 문자열을 합칠 수 있습니다. 예를 들어, 두 개의 문자열을 합치고 싶다면 다음과 같이 작성할 수 있습니다.

```Clojure
(def str1 "Hello ")
(def str2 "World!")
(println (+ str1 str2))
```

출력 결과는 다음과 같이 나올 것입니다.

```Clojure
Hello World!
```

이 외에도 더 복잡한 작업을 위해서는 "clojure.string" 라이브러리의 "join" 함수를 사용할 수도 있습니다. 이 함수는 여러 개의 문자열을 하나의 문자열로 합치는 작업을 수행합니다. 다음 예제를 통해 살펴보겠습니다.

```Clojure
(ns my-namespace.core
(:require [clojure.string :as str]))
(def coll ["Apple" "Banana" "Orange"])
(println (str/join ", " coll))
```

출력 결과는 다음과 같이 나올 것입니다.

```Clojure
Apple, Banana, Orange
```

## 깊게 파헤치기

Clojure에서 문자열 합치기를 수행할 때는 반드시 우리가 익숙한 "+"" 연산자만을 사용하는 것은 아닙니다. 예를 들어, 새로운 문자열을 생성하면서 이미 존재하는 문자열을 합치는 작업이 필요할 때는 "str" 함수를 사용할 수 있습니다. 이 함수는 "+"" 연산자와 마찬가지로 문자열을 연결하여 새로운 문자열을 생성해줍니다. 다만, "+"" 연산자와 달리 여러 개의 인자를 받을 수 있기 때문에 더 효율적으로 사용할 수 있습니다.

```Clojure
(str "Hello" " " "World!")
```

출력 결과는 다음과 같을 것입니다.

```Clojure
Hello World!
```

또한, Clojure에서는 문자열 뿐만 아니라 시퀀스(Sequence)도 "+" 연산자를 통해 합칠 수 있습니다. 시퀀스는 순서가 있는 데이터 컬렉션을 의미하며, "vec" 함수를 통해 시퀀스가 생성됩니다. 예를 들어, 다음과 같은 시퀀스를 "+"" 연산자를 사용하여 합치는 것이 가능합니다.

```Clojure
(println (+ (vec [1 2 3]) (vec [4 5 6])))
```

출력 결과는 다음과 같겠죠?

```Clojure
[1 2 3 4 5 6]
```

## 더 알아보기

번역에 일부 사용한 "clojure.string" 라이브러리에 대한 더 많은 정보는 아래 링크를 참고해주세요.

* ["clojure.string" 라이브러리 공식 문서](https://clojuredocs.org/clojure.string/join)
* ["clojure.string" 라이브러리 GitHub 페이지](https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj)
* [Clojure 공식 웹사이트](https://clojure.org/)