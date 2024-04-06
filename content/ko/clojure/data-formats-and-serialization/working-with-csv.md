---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:14.030668-07:00
description: "\uBC29\uBC95: Clojure\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0 \uB0B4\uC7A5 CSV \uD30C\uC2F1 \uAE30\uB2A5\uC774 \uC5C6\uC9C0\uB9CC, \uC774\
  \ \uBAA9\uC801\uC744 \uC704\uD574 `clojure.data.csv` \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, \uD574\uB2F9\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD504\uB85C\uC81D\uD2B8 \uC758\uC874\uC131\
  \uC5D0 \uCD94\uAC00\uD558\uC138\uC694. `project.clj`\uC5D0 \uB2E4\uC74C \uC758\uC874\
  \uC131\uC744 \uCD94\uAC00\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.694049-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uB0B4\uC7A5\
  \ CSV \uD30C\uC2F1 \uAE30\uB2A5\uC774 \uC5C6\uC9C0\uB9CC, \uC774 \uBAA9\uC801\uC744\
  \ \uC704\uD574 `clojure.data.csv` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
