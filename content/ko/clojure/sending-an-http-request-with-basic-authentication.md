---
title:                "HTTP 요청에 기본 인증을 추가하여 전송하기"
html_title:           "Clojure: HTTP 요청에 기본 인증을 추가하여 전송하기"
simple_title:         "HTTP 요청에 기본 인증을 추가하여 전송하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 왜
이번 글에서는 왜 오는 HTTP 요청에 기본 인증을 사용해야 하는지에 대한 이유를 알아보겠습니다. 기본 인증은 인터넷에서 자주 사용되는 보안 방식 중 하나로, 누군가가 링크를 클릭하거나 웹사이트에 접속하면 이를 요청하는 컴퓨터가 최초로 빈자리와 비밀번호 정보를 보내는 방식입니다. 왜 이것이 필요할까요?

# 어떻게
이제 실제로 Clojure를 사용하여 기본 인증을 사용하는 HTTP 요청을 보내는 방법을 알아보겠습니다. 먼저 HTTP 요청을 보내기 위해서 clj-http라는 라이브러리를 설치해야 합니다. 그리고 다음과 같은 코드로 HTTP 요청을 보낼 수 있습니다.
 ```Clojure
(require '[clj-http.client :as client])

(def response (client/post "https://example.com"
                :basic-auth "username" "password"))
                
(println (:resp-body response))
```
위 코드는 지정된 주소로 HTTP POST 요청을 보내고, 기본 인증으로 사용할 사용자 이름과 비밀번호를 지정합니다. 그리고 서버로부터 받은 응답을 출력합니다.

클로저에서도 GET 요청을 보내는 것도 동일한 방식으로 가능합니다. 아래의 코드를 참고하세요.
```Clojure
(def response (client/get "https://example.com"
                :basic-auth "username" "password"))
```
위의 코드는 GET 요청을 보내고, 사용자 이름과 비밀번호를 기본 인증으로 지정하며, 응답을 받아옵니다.

# 깊은 곳으로
기본 인증은 클라이언트와 서버 간의 정보를 주고받는 데 사용되는 하나의 보안 방식입니다. 이 외에도 여러 가지 보안 방식들이 존재하는데, 클로저에서는 다양한 라이브러리를 사용하여 HTTPS 요청을 보내는 것도 가능합니다. 그리고 HTTP 요청과 응답의 구조도 더 자세히 알아서 보안에 대한 이해를 높일 수 있습니다.

# 참고 자료
- [clj-http 라이브러리](https://github.com/dakrone/clj-http)
- [Clojure로 HTTPS 요청 보내기](https://medium.com/hello-world-0909/clojure%EB%A1%9C-https-%EC%9A%94%EC%B2%AD-%EB%B3%B4%EB%82%B4%EA%B8%B0-6e5b198d14e8)
- [Clojure에서 기본 인증 사용하기](https://stackoverflow.com/questions/6922666/how-do-i-access-a-website-using-basic-authentication-clojure)