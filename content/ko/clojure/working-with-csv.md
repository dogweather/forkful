---
title:                "Clojure: csv 작업하기"
simple_title:         "csv 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
CSV는 일반적으로 엑셀 또는 스프레드시트와 같은 데이터 관리 도구로 자주 사용됩니다. 따라서, Clojure 프로그래밍을 할 때 다양한 데이터를 다루기 위해 필수적으로 알아야 하는 방법입니다.

## 어떻게
CSV 파일을 Clojure에서 다루기 위해서는 먼저 [clojure.data.csv](https://github.com/clojure/data.csv) 라이브러리를 이용해야 합니다. 이 라이브러리는 CSV 파일을 파싱하고 맵 형태로 데이터를 변환하며, Clojure의 seq 데이터 형태로 변환하여 쉽게 다룰 수 있도록 해줍니다.

먼저, [clojure.java.io](https://clojure.github.io/java.io/) 라이브러리를 이용하여 파일을 열고 clojure.data.csv 라이브러리를 이용해 데이터를 파싱합니다. 다음은 파일에서 데이터를 읽어와 맵으로 변환하는 예제입니다.

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(def csv-data (csv/read-csv (io/reader "exmaple.csv")))
```

위의 예제에서 `example.csv` 파일에는 다음과 같은 데이터가 포함되어 있다고 가정합니다.
```
id, name, age
1, John, 25
2, Jane, 30
3, Emma, 20
```

파일을 읽은 후에는 `csv-data` 변수가 다음과 같이 바인딩됩니다.

```Clojure
[["id" "name" "age"]
 ["1" "John" "25"]
 ["2" "Jane" "30"]
 ["3" "Emma" "20"]]
```

이제 데이터를 쉽게 다룰 수 있습니다. 예를 들어, 다음과 같이 각 행의 id 값을 가져올 수 있습니다.

```Clojure
(def ids (rest (map first csv-data)))
```

위의 예제에서 `ids` 변수에는 `[1 2 3]` 값이 바인딩됩니다.

## 깊이있게 살펴보기
csv 라이브러리는 맵 형태로 데이터를 변환해주기 때문에 데이터를 더 쉽게 다룰 수 있습니다. 예를 들어, 위에서 언급한 `example.csv` 파일에서 데이터를 ID를 기준으로 정렬하고 싶다면 다음과 같이 할 수 있습니다.

```Clojure
(def sorted-data (sort-by (comp int first) (rest csv-data)))
```

위의 예제에서 `sorted-data` 변수에는 다음과 같이 정렬된 데이터가 바인딩됩니다.

```Clojure
[["1" "John" "25"]
 ["2" "Jane" "30"]
 ["3" "Emma" "20"]]
```

## 참고 자료
- [clojure.data.csv 라이브러리 문서](https://clojure.github.io/data.csv/)
- [clojure.java.io 라이브러리 문서](https://clojure.github.io/java.io/)
- [Clojure 공식 사이트](https://clojure.org/)
- [Clojure 공식 한국 커뮤니티 포럼](https://clojure.or.kr/)