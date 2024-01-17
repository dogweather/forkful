---
title:                "텍스트 파일 읽기"
html_title:           "Clojure: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 파일 읽기란 무엇인가요? 이것은 프로그래머가 컴퓨터에서 텍스트 파일을 읽어서 내용을 처리하는 것입니다. 많은 프로그래머들이 이를 사용하는 이유는 일반적으로 파일에서 정보를 가져오는 것이 프로그래밍의 기본 기능이기 때문입니다.

## 방법:
```Clojure
;; "resources/sample.txt" 파일에서 내용을 읽어와서 출력하기 
(with-open [rdr (clojure.java.io/reader "resources/sample.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

코드 실행 결과:
This is a sample text file.
It contains some random text.
```

```Clojure
;; "resources/sample.json" 파일에서 내용을 읽어와서 Clojure 데이터 타입으로 변환하기
(clojure.data.json/read-str (slurp "resources/sample.json"))
 
코드 실행 결과:
{:key1 "value1", :key2 2, :key3 true}
```

```Clojure
;; "resources/sample.csv" 파일에서 내용을 읽어와서 행 단위로 처리하기
(require '[clojure-csv.core :as csv])
(csv/parse-csv (slurp "resources/sample.csv"))
 
코드 실행 결과:
[["col1" "col2" "col3"] ["row1" "row2" "row3"]]
```

## 깊은 곳 더 들어가기:
텍스트 파일 읽기는 과거부터 프로그래밍에서 중요한 요소였습니다. 이전에는 다른 언어에서 파일에서 정보를 읽고 처리하는 과정이 복잡했지만, Clojure는 간단하고 강력한 함수들을 제공하여 파일 처리를 쉽게 할 수 있게 만들어줍니다. 또한 파일 읽기 대신 데이터베이스와 같은 다른 방법을 사용할 수도 있습니다. 파일 읽기는 주로 로그 파일 분석이나 초기 데이터 로딩에 사용되며, Clojure의 다양한 라이브러리와 함께 다양한 형태로 사용될 수 있습니다.

## 관련 자료:
- [Clojure 함수 레퍼런스](https://clojuredocs.org/clojure.java.io/reader)
- [Clojure-CSV 라이브러리](https://github.com/clojure/data.csv)
- [Clojure 데이터 JSON 변환](https://github.com/clojure/data.json)