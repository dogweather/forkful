---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 데이터를 저장하고 공유할 때 쓰는 표준 형식이에요. 간단하고 범용적인 포맷 덕분에 프로그래머는 시스템 간 호환성을 높이고 데이터 처리를 용이하게 할 수 있죠.

## How to:
```Clojure
;; 데이터 파싱하기
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "example.csv")]
  (doall (csv/read-csv reader)))

;; 데이터 쓰기
(with-open [writer (io/writer "example_out.csv")]
  (csv/write-csv writer [["name" "age" "city"] ["Jane" "25" "New York"] ["Joe" "35" "Los Angeles"]]))
```
샘플 출력:
```Clojure
;; 파싱된 데이터
(["name" "age" "city"] ["Jane" "25" "New York"] ["Joe" "35" "Los Angeles"])
;; example_out.csv 파일 확인
name,age,city
Jane,25,New York
Joe,35,Los Angeles
```

## Deep Dive
CSV 포맷은 1970년대부터 쓰이고 있어요. 간단한 텍스트 포맷으로, 엑셀이나 데이터베이스 시스템과 호환이 쉬워요. JSON이나 XML 같은 데이터 포맷이 대안이 될 수 있지만, CSV는 구조가 단순하고 읽고 쓰기 쉬운 장점이 있죠. Clojure에서는 `clojure.data.csv` 라이브러리를 주로 사용하며, 이는 내부적으로 Java의 기능을 활용해서 CSV 파일을 쉽게 다룰 수 있게 해줍니다.

## See Also
- 공식 Clojure CSV 라이브러리 문서: [clojure.data.csv](https://github.com/clojure/data.csv)
- ClojureDocs, CSV 예제: [ClojureDocs CSV](https://clojuredocs.org/clojure.data.csv)
