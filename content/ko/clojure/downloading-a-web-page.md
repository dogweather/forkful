---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이며, 왜?

웹페이지를 다운로드하려면 웹서버로부터 임의의 웹 문서 데이터를 받아야 합니다. 이것이 필요한 이유는 개발자가 웹 페이지의 콘텐츠를 분석하거나, 저장하거나, 재사용할 수 있게 해줍니다.

## 어떻게:

아래는 Clojure를 사용해 웹 페이지를 다운로드하는 예시입니다. 
```Clojure
(require '[clojure.java.io :as io])

(defn download-page [url]
  (with-open [outstream (io/output-stream "myfile.html")]
    (.write outstream (.getBytes (slurp url)))))
```
이 코드는 웹 페이지의 내용을 가져와 "myfile.html"라는 파일에 저장합니다.

## 디프 다이브:

웹 페이지 다운로드는 HTTP 통신의 기본이며, 초기 웹 브라우징의 핵심 요소였습니다. Clojure와 같은 다양한 언어를 이용하여 이를 구현 가능합니다. 다양한 대안으로는 requests 라이브러리나 wget, curl 같은 커맨드 라인 툴이 있습니다. Clojure에서는 `slurp` 함수를 사용하여 서버로부터 데이터를 읽어 들이고, 읽어온 데이터는 `with-open`과 `write`를 이용하여 파일에 저장합니다.

## 또한 참조하세요:

웹 크롤링과 관련된 더 복잡한 예제는 다음 링크들을 참조하세요:
- [Web Scraping with Clojure](https://kimh.github.io/clojure-by-example/#web-scraping)
- [Introduction to Web Scraping with Clojure](https://realpython.com/tutorials/web-scraping/)
- [Clojure Cookbook - Reading from URLs](https://www.clojure-cookbook.com/posts_reading_from_url.html)