---
title:                "CSV와 함께 작업하기"
date:                  2024-02-03T19:19:14.030668-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV (쉼표로 구분된 값) 파일을 다루는 것은 스프레드시트 데이터와 유사한 행과 열로 구성된 텍스트 데이터를 파싱하고 생성하는 과정을 포함합니다. 이 과정은 CSV가 경량이면서 상호운용 가능한 포맷으로 널리 채택되었기 때문에, 애플리케이션, 데이터베이스 간의 데이터 교환과 데이터 변환 작업에 필수적입니다.

## 방법:

### CSV 파일 읽기
Clojure는 표준 라이브러리에 내장 CSV 파싱 기능이 없지만, 이 목적을 위해 `clojure.data.csv` 라이브러리를 사용할 수 있습니다. 먼저, 해당 라이브러리를 프로젝트 의존성에 추가하세요.

`project.clj`에 다음 의존성을 추가합니다:
```clojure
[clojure.data.csv "1.0.0"]
```
CSV 파일을 읽고 각 행을 출력하려면:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
이것은 CSV의 각 행을 Clojure 벡터로 출력합니다.

### CSV 파일에 쓰기
CSV 파일에 데이터를 쓰려면, 같은 `clojure.data.csv` 라이브러리를 사용할 수 있습니다:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
이것은 `outputfile.csv`를 생성하거나 덮어쓰며, 지정된 데이터로 채웁니다.

### 타사 라이브러리 사용하기: `clojure.data.csv`

`clojure.data.csv`는 Clojure에서 CSV 처리를 위해 가장 직관적인 라이브러리일 수 있지만, 특수 문자나 비전통적인 구분자가 포함된 CSV를 다루는 등 더 복잡한 작업을 위해서는 생태계 내 추가 옵션을 탐색하거나 Apache Commons CSV와 같은 Java 상호운용 라이브러리를 고려할 수 있습니다. 그러나 Clojure에서의 대부분의 표준 CSV 처리 작업을 위해, `clojure.data.csv`는 단순하고 효과적인 도구 세트를 제공합니다.
