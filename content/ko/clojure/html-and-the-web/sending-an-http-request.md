---
title:                "HTTP 요청 보내기"
aliases:
- /ko/clojure/sending-an-http-request.md
date:                  2024-01-20T17:59:13.807782-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTTP 요청은 웹 서버와 정보를 교환하기 위한 방법이다. 프로그래머는 데이터를 가져오거나 웹 서비스에 작업을 요청하기 위해 이를 사용한다.

## How to: (방법:)
Clojure에서 `clj-http` 라이브러리를 사용하여 HTTP 요청을 보낸다.

```Clojure
;; clj-http 라이브러리 추가
(require '[clj-http.client :as client])

;; GET 요청 보내기
(def response (client/get "https://httpbin.org/get"))
(println response)

;; POST 요청 보내기
(def post-response (client/post "https://httpbin.org/post" {:form-params {:key "value"}}))
(println post-response)
```

예상 출력값은 다음과 같다:

```Clojure
;; GET 요청의 응답
{:status 200, :headers {...}, :body "..."}
;; POST 요청의 응답
{:status 200, :headers {...}, :body "..."}
```

## Deep Dive (심층 분석)
HTTP 요청은 웹의 근간이며, 1991년 HTTP/0.9으로 처음 등장했다. Clojure에서는 `clj-http` 외에도 `http-kit`나 `aleph` 같은 라이브러리로 HTTP 요청을 보낼 수 있다. `clj-http`는 Java의 Apache HttpClient를 기반으로 하며, 동기/비동기 요청, 다양한 HTTP 메소드 지원, 인증과 같은 기능을 제공한다. 성능이 중요한 상황에서는 네이티브 라이브러리를 선택할 수도 있다.

## See Also (참고자료)
- clj-http GitHub 페이지: https://github.com/dakrone/clj-http
- HTTP 요청에 대한 Clojure 가이드: https://clojure.org/guides/http_client
- 비교: 다른 Clojure HTTP 라이브러리들: https://www.clojure-toolbox.com/
