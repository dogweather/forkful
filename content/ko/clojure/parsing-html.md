---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱이란, HTML 코드 내부의 요소를 분석하고 구조화하는 과정을 의미합니다. 프로그래머들이 이를 사용하는 이유는 웹 페이지에서 필요한 정보를 추출하기 위해서입니다. 

## 어떻게 하나요:

아래는 'clojure.data.xml' 라이브러리를 사용해 HTML을 파싱하는 Clojure 코드 예제입니다. 

```Clojure
(ns html-parsing.core
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(defn parse-html [filepath]
  (with-open [reader (io/reader filepath)]
    (xml/parse reader)))

(defn -main [& args]
  (println (parse-html "/path/to/your/file.html")))
```

실행 결과:

```Clojure
{:tag :html, :attrs {}, :content [...]}
```

## Deep Dive:

HTML 파싱에 대해 더 알아볼 때 몇 가지 요소들을 고려해 보아야 합니다.

1. **역사적 맥락** : HTML 파싱은 웹 크롤링, 웹 스크랩핑, 웹 데이터 마이닝 등 다양한 응용분야에서 중요하게 사용되어 왔습니다. 

2. **대안** : 'clojure.data.xml' 외에도 'jsoup', 'webdriver', 'htmlcleaner' 등의 다른 라이브러리들도 HTML 파싱에 사용할 수 있습니다. 

3. **구현 세부 정보** : 'clojure.data.xml'는 SAX (Simple API for XML)를 구현하여 파싱을 수행합니다. 이를 통해 대용량 XML과 HTML 문서를 효율적으로 처리할 수 있습니다.

## 참고자료 :

아래 링크들을 통해 HTML 파싱에 대한 추가 정보를 얻을 수 있습니다.

1. [Clojure 공식 문서](https://clojure.org/)
2. ['clojure.data.xml' GitHub page](https://github.com/clojure/data.xml)
3. [Jsoup Documentation](https://jsoup.org/)
4. [HtmlCleaner Documentation](https://htmlcleaner.sourceforge.io/)