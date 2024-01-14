---
title:                "Clojure: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지를 다운로드하는 이유는 다양합니다. 인터넷에서 무엇을 찾고있기 때문에 웹 페이지를 다운로드하고 분석하려는 경우가 있을 수 있습니다. 또는 자신의 웹 서비스에서 웹 페이지의 내용을 스크랩하고 그 정보를 자체적으로 사용하고 싶을 때도 있을 수 있습니다.

## 사용 방법

웹 페이지를 다운로드하려면 먼저 인터넷 연결이 필요합니다. 그런 다음 `clojure.java.io` 라이브러리에서 제공하는 `copy` 함수를 사용하면 간단하게 웹 페이지를 다운로드 할 수 있습니다. 다음은 예제 코드와 함께 다운로드한 웹 페이지의 내용을 출력하는 코드입니다.

```Clojure
(use 'clojure.java.io)

(def url "https://example.com")

(def content (slurp (input-stream (clojure.java.io/copy url (java.net.URL. url) {:method :get}))))

(println content)
```

위의 코드를 실행하면 `content` 변수에 해당 URL의 웹 페이지 내용이 문자열 형태로 저장됩니다. 만약 파일로 저장하고 싶다면 `spit` 함수를 사용하여 저장할 수도 있습니다.

```Clojure
(spit "example.html" content)
```

## 깊게 들어가기

웹 페이지를 다운로드하는 것 이상의 일을 하기 위해서는 웹 페이지를 분석하는 것도 중요합니다. 예를 들어, 웹 페이지의 특정 부분만을 스크랩하고 싶다면 `hiccup` 라이브러리를 사용하여 HTML 문서를 구문 분석할 수 있습니다. 또는 웹 페이지의 URL을 수집하고 웹 크롤러를 만들고 싶다면 `enlive` 라이브러리를 사용하여 원하는 정보를 추출하고 탐색할 수 있습니다.

## 참고

- `clojure.java.io`: http://clojuredocs.org/clojure.java.io
- `hiccup`: http://weavejester.github.io/hiccup/
- `enlive`: https://github.com/cgrand/enlive