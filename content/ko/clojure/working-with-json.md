---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 데이터 교환 포맷입니다. 간단하고 가벼워서 Clojure 프로그래머들이 웹서비스에서 데이터를 주고받을 때 자주 사용합니다.

## How to: (어떻게 하나요?)
Clojure에서 JSON을 다루려면 `cheshire` 라이브러리를 사용하세요. 설치 후 JSON 읽기와 쓰기 예제:

```Clojure
;; Cheshire 라이브러리 추가
(require '[cheshire.core :as json])

;; JSON 문자열을 Clojure 맵으로 파싱
(json/parse-string "{\"name\":\"Clojure\", \"type\":\"Lisp\"}")
;; 출력: {"name" "Clojure", "type" "Lisp"}

;; Clojure 맵을 JSON 문자열로 변환
(json/generate-string {:name "Clojure" :type "Lisp"})
;; 출력: "{\"name\":\"Clojure\",\"type\":\"Lisp\"}"
```

## Deep Dive (심층 분석)
JSON은 JavaScript의 객체 표기법에서 유래했습니다. XML과 비교하면 더 읽기 쉽고 쓰기 쉽습니다. Clojure에서는 `cheshire` 라이브러리가 가장 인기 있는데, 이는 내부적으로 Jackson 라이브러리를 사용하여 JSON을 처리합니다. 대안으로 `clojure.data.json` 라이브러리도 있지만, 성능과 기능 면에서 `cheshire`가 앞섭니다.

## See Also (참고자료)
- Cheshire 공식 문서: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- clojure.data.json 라이브러리: [https://github.com/clojure/data.json](https://github.com/clojure/data.json)
- Clojure 공식 사이트: [https://clojure.org/](https://clojure.org/)
