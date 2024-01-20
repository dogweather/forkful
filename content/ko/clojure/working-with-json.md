---
title:                "JSON 작업하기"
html_title:           "Clojure: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

JSON 작업이란 무엇일까요? JSON은 JavaScript Object Notation의 약자로서 데이터를 저장하고 전송하기 위한 포맷 중 하나입니다. 프로그래머들은 JSON을 사용하는 이유는 데이터를 효율적으로 처리하고 다른 시스템과의 상호작용을 용이하게 하기 위해서입니다.

## 하는 법:

```Clojure
;; JSON 데이터 생성하기
(def data {:name "Jane" :age 25 :occupation "Developer"})

;; 데이터를 JSON 문자열로 변환하기
(def json-data (json/write-str data))

;; 결과 출력
(print json-data)

;; JSON 문자열 파싱하기
(def parsed-data (json/parse-string json-data))

;; 결과 출력
(print parsed-data)
```

## 깊이 들어가보기:

JSON은 1999년에 더글라스 크록포드(Douglas Crockford)가 개발한 데이터 교환 포맷입니다. XML 대비 간단하고 가벼우며 웹 어플리케이션에서 빠른 처리가 가능하기 때문에 널리 사용되고 있습니다. 또한 대부분의 프로그래밍 언어에서 JSON을 지원하고 있으며, XML처럼 복잡한 구조가 아니기 때문에 쉽게 다룰 수 있습니다.

## 관련 자료:

- [JSON 공식 사이트](https://www.json.org/)
- [Clojure JSON 라이브러리](https://github.com/clojure/data.json)