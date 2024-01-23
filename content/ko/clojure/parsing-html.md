---
title:                "HTML 파싱"
date:                  2024-01-20T15:30:58.493857-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜인가?)
HTML을 파싱한다는 것은 HTML 문서에서 정보를 추출하는 것을 말합니다. 프로그래머들은 데이터를 자동으로 수집하거나 콘텐츠를 가공할 목적으로 이 작업을 합니다.

## How to: (어떻게 하나:)
```clojure
;; Clojure에서 HTML 파싱하기

;; Enlive 라이브러리 추가
(require '[net.cgrand.enlive-html :as html])

;; HTML 문서 불러오기
(def page (html/html-resource (java.net.URL. "https://example.com")))

;; 특정 요소 추출하기
(defn get-titles [page]
  (html/select page [:title]))

;; 출력 예시
(println (get-titles page))
;; => ({:tag :title, :attrs nil, :content ["Example Domain"]})
```

## Deep Dive (심층 탐구)
HTML 파싱은 1990년대 초 웹 탄생 이래로 필요했습니다. 'Enlive'는 Clojure에서 매력적인 HTML 파싱 라이브러리 중 하나입니다. Enlive는 DOM을 제어하기보다는 쿼리와 변환을 이용해 데이터를 추출합니다. HTML을 파싱하는 다른 방법으로는 Jsoup, Hickory 등이 있지만, Enlive는 Clojure적인 접근 방식을 수용합니다. 

## See Also (더 보기)
- Enlive 공식 문서: [https://github.com/cgrand/enlive](https://github.com/cgrand/enlive)
- Clojure에 대한 더 깊은 학습을 위한 공식 가이드: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Clojure HTML 파싱을 위한 다른 라이브러리, Hickory: [https://github.com/davidsantiago/hickory](https://github.com/davidsantiago/hickory)
