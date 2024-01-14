---
title:                "Clojure: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-json.md"
---

{{< edit_this_page >}}

# Why
JSON은 현대 프로그래밍에서 중요한 역할을 합니다. 많은 언어들이 JSON을 자료 형식으로 지원하고, 웹 프로그래밍에서는 데이터를 전송하는 데에 널리 사용됩니다. 따라서 Clojure에서도 JSON을 배우는 것은 필수적입니다.

# How To
```Clojure
;; JSON 문자열 파싱하기
(require '[cheshire.core :as json])
(json/parse-string "{\"name\":\"John\", \"age\": 25, \"hobbies\":[\"reading\", \"coding\"]}") ;; {:name "John", :age 25, :hobbies ["reading" "coding"]}

;; map을 JSON 문자열로 변환하기
(require '[cheshire.core :as json])
(json/generate-string {:name "Jane", :age 30, :hobbies ["hiking", "painting"]}) ;; "{\"name\":\"Jane\", \"age\":30, \"hobbies\":[\"hiking\", \"painting\"]}"

;; JSON 파일 읽어오기
(require '[clojure.java.io :as io])
(require '[cheshire.core :as json])
(with-open [r (io/reader "sample.json")]
  (let [data (json/parse-stream r)]
    (println data))) ;; {:name "Sam", :age 35, :hobbies ["cooking", "gardening"]}

;; JSON 파일 쓰기
(require '[clojure.java.io :as io])
(require '[cheshire.core :as json])
(with-open [w (io/writer "new_sample.json")]
  (json/generate-stream {:name "Alex", :age 28, :hobbies ["photography", "hiking"]} w))

;; nested한 JSON 다루기
(require '[cheshire.core :as json])
(def sample {:employees [{:name "John", :age 25}, {:name "Jane", :age 30}]})
(json/generate-string sample) ;; "{\"employees\":[{\"name\":\"John\", \"age\":25}, {\"name\":\"Jane\", \"age\":30}]}"
```

# Deep Dive
Clojure에서는 JSON을 다루는 데에 쉽고 강력한 라이브러리인 Cheshire를 제공합니다. 이 라이브러리는 가장 널리 사용되는 JSON 라이브러리 중 하나이며, 매우 빠르고 유연합니다. 또한, Clojure의 특징인 함수형 프로그래밍 스타일을 따르기 때문에, 데이터 변환을 위해 함수를 조합할 수 있어 더 깔끔하고 간결한 코드를 작성할 수 있습니다.

# See Also
- [Cheshire library](https://github.com/dakrone/cheshire)
- [Official Clojure documentation on JSON](https://clojure.org/reference/data_structures#_json)