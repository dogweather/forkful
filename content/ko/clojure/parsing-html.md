---
title:                "HTML 파싱"
aliases:
- ko/clojure/parsing-html.md
date:                  2024-02-03T19:11:43.253014-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Clojure에서 HTML 파싱은 HTML 문서에서 정보를 프로그래매틱하게 추출하는 과정을 말합니다. 프로그래머들은 웹 콘텐츠에 접근, 조작 또는 모니터링하고, 작업을 자동화하거나 애플리케이션에 데이터를 공급하기 위해 이 작업을 수행합니다.

## 어떻게:

Clojure는 내장 HTML 파싱 기능이 없지만, Java 라이브러리 또는 Clojure 래퍼들인 `enlive` 또는 `hickory`를 활용할 수 있습니다. 두 가지 모두 사용하는 방법은 다음과 같습니다:

### Enlive 사용하기:

Enlive는 HTML 파싱 및 웹 스크레이핑에 인기 있는 선택입니다. 먼저, 프로젝트 의존성에 포함시키세요:

```clojure
[net.cgrand/enlive "1.1.6"]
```

그런 다음, HTML을 파싱하고 탐색할 수 있습니다:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

이 코드는 HTML 페이지를 가져와 클래스 `some-class`를 가진 모든 `<div>` 요소를 선택합니다.

출력은 다음과 같을 수 있습니다:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["여기 몇 가지 내용이 있습니다."]})
```

### Hickory 사용하기:

Hickory는 Clojure에서 작업하기 쉬운 형식으로 HTML을 파싱하는 방법을 제공합니다. 프로젝트 의존성에 Hickory를 추가하세요:

```clojure
[hickory "0.7.1"]
```

간단한 예는 다음과 같습니다:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; HTML을 Hickory 형식으로 파싱
(let [doc (hickory/parse "<html><body><div id='main'>안녕, 세계!</div></body></html>")]
  ;; id가 'main'인 div를 선택
  (select/select (select/id "main") doc))
```

이 코드는 간단한 HTML 문자열을 파싱하고 CSS 선택자를 사용하여 ID가 `main`인 `div`를 찾습니다.

샘플 출력:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["안녕, 세계!"]}]
```

`enlive`와 `hickory` 모두 Clojure에서 HTML 파싱을 위한 강력한 솔루션을 제공하며, `enlive`는 템플릿에, `hickory`는 데이터 변환에 더 중점을 둡니다.
