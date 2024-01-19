---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요할까요?
HTTP 요청 전송은 서버에 정보를 요청하거나 보내는 방법을 말합니다. 프로그래머들은 다른 애플리케이션의 서비스를 이용하거나 정보를 요청하거나 수정할 때 이를 사용합니다.

## 어떻게 사용하나요?
아래 코드 블럭은 Clojure에서 HTTP GET 요청을 보내는 예시입니다.

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com")]
  (println (:status response) (:body response)))
```

이것은 http://example.com으로 GET 요청을 보내고 그 응답의 상태와 본문을 출력합니다.

## 깊이 들어가 보기
HTTP 요청보내기는 웹에서 개발을 하기 위한 기본 중의 기본입니다. 이 역사는 웹의 역사만큼이나 오래되었습니다. 대안으로는 Java나 Python과 같은 다른 언어에서의 기본적인 라이브러리나 Node.js, Go 등 다양한 언어에서 지원되는 다양한 HTTP 클라이언트 라이브러리들이 있습니다.

Clojure에서는 가장 간단한 방법으로는 라이브러리 'clj-http'를 사용하는 것이고, 이 라이브러리에서는 'get', 'post', 'put' 등 다양한 HTTP 메서드를 지원합니다.

## 참고 목록
- HTTP 요청에 대한 자세한 문서: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- 'clj-http'라이브러리 깃허브 페이지: [Github](https://github.com/dakrone/clj-http)
- Clojure에 대한 자세한 설명: [Official Clojure Documentation](https://clojure.org/guides/getting_started)