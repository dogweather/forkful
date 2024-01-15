---
title:                "json과 함께 작업하기"
html_title:           "Clojure: json과 함께 작업하기"
simple_title:         "json과 함께 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-json.md"
---

{{< edit_this_page >}}

README

## 왜 JSON을 다루는 것일까요?

JSON은 현대의 프로그래밍에서 매우 중요한 역할을 합니다. 대부분의 프로그래밍 언어에서 JSON을 사용하여 데이터를 교환하고 저장하는 데 이용합니다. Clojure에서도 마찬가지로 JSON을 다루는 법을 알아야합니다.

## 어떻게 하나요?

Clojure에서 JSON을 다루는 가장 일반적인 방법은 clojure.data.json 라이브러리를 사용하는 것입니다. 이 라이브러리를 사용하면 간편하게 JSON을 읽고 쓸 수 있습니다.

예제를 살펴보겠습니다.

```Clojure
;; 라이브러리를 불러오기
(ns my-namespace
  (:require [clojure.data.json :as json]))

;; 데이터를 JSON으로 변환
(def data (json/write-str {:name "John" :age 25}))

;; 변환된 JSON 출력
(println data)

;; JSON을 데이터로 변환
(def parsed-data (json/read-str data))

;; 변환된 데이터 출력
(println parsed-data)
```

#### 출력 결과:

```Clojure
{"name":"John","age":25}
{:name "John", :age 25}
```

위의 예제에서는 clojure.data.json 라이브러리를 사용하여 데이터를 JSON으로 변환하고 다시 데이터로 변환하는 방법을 보여주었습니다. 보시다시피 간단하고 쉽게 JSON을 다룰 수 있습니다.

## 깊이있게 공부해보기

위의 예제에서는 단순하게 json/write-str과 json/read-str 함수를 사용하여 JSON을 다루는 방법을 소개했습니다. 하지만 Clojure에서는 이 외에도 다양한 라이브러리가 존재합니다.

예를 들어, cheshire 라이브러리는 JSON 데이터를 Clojure의 맵으로 바꿔주는 함수를 제공합니다. 그리고 data.json 라이브러리는 Clojure 데이터를 쉽게 JSON으로 변환하고 다시 맵으로 변환하는 기능을 제공합니다.

따라서 여러분은 자신이 사용하고 있는 프로젝트에 맞는 라이브러리를 선택하여 사용할 수 있습니다. 이번 기회에 다양한 라이브러리를 탐색하며 대체적으로 어떤 함수나 기능이 제공되는지 알아보세요.

## See Also

- [clojure.data.json 라이브러리 문서](https://clojuredocs.org/clojure.data.json)
- [Clojure로 JSON 다루는 법 알아보기](https://www.baeldung.com/clojure-json)
- [Cheshire 라이브러리 문서](https://github.com/dakrone/cheshire)