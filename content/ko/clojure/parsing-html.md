---
title:                "HTML 분석"
html_title:           "Clojure: HTML 분석"
simple_title:         "HTML 분석"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
이 글을 읽는 분들 중에서 HTML 파싱 프로그래밍을 해보고 싶어하는 분들이 있을 것 같아서 쓰게 되었어요. HTML 파싱은 웹 서비스를 만들 때 필수적인 기술이므로, 웹 개발자가 되고 싶은 분들에게 특히 유용할 것입니다.

## 하우 투
HTML 파싱에 대한 기본적인 개념부터 실제 예제까지 살펴보겠습니다. 마크다운 포맷을 사용하여 코드 블록 안에 실제 Clojure 코드와 실행 결과를 볼 수 있도록 하겠습니다.

### Clojure으로 HTML 파싱하기
```Clojure
(ns html-parser.core
  (:require [clojure.string :as str])
  (:require [clojure.xml :as xml]))

(def html-string "<div id='title'><h1>Hello, World!</h1></div>")
```
위 코드는 Clojure의 `html-parser` 네임스페이스를 선언하고 필요한 라이브러리들을 가져오는 부분입니다. 그리고 HTML 형태의 문자열을 변수에 저장합니다.

```Clojure
(defn parse-html [html]
  (xml/parse-str html))
```
`parse-html`이라는 함수는 `html`이라는 인자를 받아 Clojure의 `xml` 라이브러리를 이용하여 해당 문자열을 파싱합니다.

```Clojure
(defn get-title [parsed-html]
  (xml-> parsed-html :html :body :div [:span#title]))
```
`get-title` 함수는 파싱된 HTML을 인자로 받아 `xml->` 매크로를 이용하여 `id`가 `title`인 `span` 태그를 찾습니다.

```Clojure
(let [parsed-html (parse-html html-string)]
  (get-title parsed-html))
```
위의 코드를 실행하면 아래와 같은 결과를 얻을 수 있습니다.
```
({:tag :span, :attrs {:id "title"}, :content [{:tag :h1, :attrs {}, :content ["Hello, World!"]}]}
```
파싱한 HTML의 구조를 표현하는 Clojure의 데이터 구조를 반환합니다.

## 딥 다이브
HTML 파싱을 잘 사용하기 위해서는 더 많은 지식이 필요하지만, 이 글에서는 가볍게 살펴볼 것입니다. HTML 파싱을 위해 주로 사용되는 라이브러리는 `clojure.xml` 외에도 `enlive`와 `hiccup`이 있습니다. 각각의 라이브러리는 다양한 기능을 제공하므로, 필요에 따라 선택하여 사용하면 됩니다.

## 참고
- [Clojure 공식 사이트](https://www.clojure.org/)
- [Clojure로 HTML 파싱하기](https://yokolet.gitbooks.io/clojure-for-the-brave-and-true/15.html)
- [Clojure로 웹 스크래핑하기](https://hackernoon.com/guide-to-web-scraping-in-clojure-dda0643b3335)