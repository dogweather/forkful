---
title:                "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 일은 우리가 웹 브라우저를 통해 웹 사이트에 접속할 때 매우 중요합니다. 이를 통해 서버와 클라이언트 간의 통신이 가능해지기 때문입니다. Clojure 프로그래밍에서 HTTP 요청을 보내는 방법을 배우기 전에 그 이유에 대해 간단히 살펴보겠습니다.

## 하우 투

Clojure에서 HTTP 요청을 보내기 위해서는 먼저 클라이언트 라이브러리를 사용해야 합니다. 예를 들어, 여러분이 클라이언트 라이브러리 중에서 Http Client를 선택했다고 가정해봅시다. 그렇다면 다음과 같이 라이브러리를 추가하고 namespace를 선언해주어야 합니다.

```Clojure
[http-client "0.1.0"]
```

```Clojure
(require '[http-client.core :as http])
```

이제 HTTP 요청을 보내는 방법은 매우 간단합니다. 다음과 같이 `http/get` 함수를 사용하면 됩니다.

```Clojure
(http/get "http://www.example.com")
```

그리고 `response`를 출력하면 다음과 같은 sample output을 얻을 수 있습니다.

```Clojure
{:status 200
 :headers {"content-type" "text/html; charset=UTF-8"}
 :body <!DOCTYPE html>
<html>
<head>
<title>Welcome to Example Corporation</title>
</head>
<body>
<h1>Welcome!</h1>
<p>Hello, World!</p>
</body>
</html>}
```

## 깊이 파고들기

보다 복잡한 HTTP 요청을 보내기 위해선, 각종 매개변수를 추가해주어야 합니다. 예를 들어, 사용자가 입력한 정보를 서버로 전송하거나 페이지의 특정 부분만 가져오는 등의 작업을 해야할 때가 있습니다. 이럴 때는 `http/get` 함수 대신 `http/post` 함수를 사용해야 합니다.

```Clojure
(http/post "http://www.example.com" {:form-params {"username" "example_user",
                                                   "password" "example_password"}})
```

더 자세한 내용은 [이 공식 문서](https://github.com/tailrecursion/cljs-http)를 참고하시기 바랍니다.

## 또 다른 참고 자료

- [Clojure 공식 홈페이지](https://clojure.org/)
- [뉴비를 위한 Clojure 프로그래밍 가이드](https://clojure.org/guides/getting_started)
- [HTTP 요청에 대한 자세한 설명](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)