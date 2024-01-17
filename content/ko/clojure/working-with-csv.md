---
title:                "CSV 파일 사용하기"
html_title:           "Clojure: CSV 파일 사용하기"
simple_title:         "CSV 파일 사용하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

CSV 작업이란 무엇일까요? 이것은 데이터를 스프레드시트나 데이터베이스에 저장할 수 있는 표 형식의 파일 형식입니다. 프로그래머가 CSV를 다루는 이유는 데이터를 쉽고 효율적으로 구성하고 관리하기 위해서입니다.

## 방법:

```Clojure
;; CSV 파일 로드하기
(require '[clojure.data.csv :as csv])

;; 파일의 내용 읽어오기
(with-open [reader (clojure.java.io/reader "file.csv")]
  (csv/read-csv reader))

;; 데이터베이스에 저장하기
(with-open [writer (clojure.java.io/writer "file.csv")]
  (csv/write-csv writer data))
```

## 심층 분석

CSV 작업은 전통적인 데이터 저장과 교환 방식으로 문서 형식이 아닌 데이터를 메모리에서 처리하는 방식입니다. 이를 다루는 대안에는 XML, JSON 등이 있으며 이들은 구조화된 데이터를 다룰 때 더 나은 선택이 될 수 있습니다. CSV 작업은 `clojure.data.csv` 라이브러리를 사용하여 구현할 수 있으며, 표준적인 형식과 커스텀 형식 모두 지원합니다.

## 관련 자료

[clojure.data.csv 라이브러리 문서](https://clojure.github.io/data.csv/)