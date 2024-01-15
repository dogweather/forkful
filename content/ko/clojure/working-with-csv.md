---
title:                "CSV 작업하기"
html_title:           "Clojure: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV를 다루는 것이 유익한가요?

CSV는 엑셀과 같은 스프레드시트 프로그램에서 자주 사용되는 데이터 형식입니다. 따라서 CSV 파일을 처리하는 것은 비즈니스에서 필수적인 작업이 될 수 있습니다. 또한 Clojure에서 CSV를 다루는 것은 매우 간단하고 빠르며 유용합니다.

## 어떻게 하면 될까요?

CSV를 다루는 데에는 두 가지 주요 방법이 있습니다. 첫 번째 방법은 Clojure 라이브러리인 "clojure-csv"를 사용하는 것입니다. 이 라이브러리는 CSV 파일을 읽고 쓰는 데에 필요한 모든 기능을 제공합니다. 예를 들어, CSV 파일을 읽어서 데이터를 맵으로 변환하는 코드는 다음과 같습니다. 

```Clojure
(require '[clojure-csv.core :as csv])

(csv/parse-csv "file.csv") ; CSV 파일을 파싱하여 한 줄씩 맵으로 변환
```

두 번째 방법은 Clojure의 내장 함수인 "clojure.string"을 사용하는 것입니다. 이 방법은 더욱 간단하지만, 기본적인 기능만 제공합니다. 예를 들어, CSV 파일을 문자열로 읽은 뒤 줄마다 분리하여 리스트로 변환하는 코드는 다음과 같습니다.

```Clojure
(require '[clojure.java.io :as io])
(require '[clojure.string :as string])

(string/split (slurp (io/file "file.csv")) #"\r\n") ; CSV 파일을 문자열로 읽고 줄마다 분리하여 리스트로 변환
```

## 딥 다이브

CSV 파일을 읽고 쓰는 것 외에도, Clojure에서는 다양한 방식으로 CSV를 다룰 수 있습니다. 예를 들어, "data.csv" 파일을 분석하여 평균 값을 구하는 코드는 다음과 같습니다.

```Clojure
(require '[clojure.string :as string])
(require '[clojure.java.io :as io])
(require '[clojure.math :as math])

(defn get-data [file]
  ; CSV 파일을 읽고 데이터를 리스트로 변환
  (->> (slurp file) (string/split #"\r\n") (map string/split #",") (map #(map #(Float/parseFloat %) %))))

(defn get-average [data]
  ; 데이터의 평균 값을 구하는 함수
  (/ (apply + data) (count data)))

(with-open [file (io/reader "data.csv")]
  (->> (get-data file) (map get-average))) ; data.csv 파일에서 데이터를 읽고 평균 값을 구하는 코드
```

딥 다이브를 위해 추가적인 자료를 참고하고 싶다면 아래 링크를 확인해 보세요.

## See Also
- [Clojure Docs](https://clojure.org/)
- [clojure-csv 라이브러리](https://github.com/danlentz/clojure-csv)
- [Clojure for the Brave and True](https://www.braveclojure.com/)