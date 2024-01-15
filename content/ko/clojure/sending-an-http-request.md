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

## 왜

선택적인 API와 통신하거나 웹 페이지에 요청을 보내기 위해 HTTP 요청을 보내는 법을 배워야 할 때가 있습니다.

## 방법

HTTP 클라이언트 라이브러리를 사용하여 Clojure에서 HTTP 요청을 보내는 방법을 알아보겠습니다.

```Clojure
(require '[clj-http.client :as client])
```

우리가 보낼 요청의 기본적인 형태는 다음과 같습니다:

```Clojure
(client/request {:method :get :url "https://www.example.com"})
```

위의 코드는 :method를 통해 어떤 유형의 요청을 할지, :url을 통해 어느 사이트로 요청을 보낼지를 지정합니다. 이제 이 요청에 대한 응답을 받아서 볼 수 있습니다.

```Clojure
(client/request {:method :get :url "https://www.example.com"})
;;=> {:status 200, :headers {"Content-Type" "text/html"}, :body "<html><body>Hello world!</body></html>"}
```

우리는 또한 옵션으로 request의 바디에 파라미터를 추가할 수 있습니다.

```Clojure
(client/request {:method :post :url "https://www.example.com/login" :form-params {:username "user" :password "pass"}})
```

위의 코드는 POST 요청을 보내며, :form-params를 통해 전달할 파라미터를 설정합니다. 이제 이를 보낸 결과를 받아 볼 수 있습니다.

```Clojure
(client/request {:method :post :url "https://www.example.com/login" :form-params {:username "user" :password "pass"}})
;;=> {:status 200, :headers {"Content-Type" "application/json"}, :body "{\"success\": true, \"message\": \"Login successful.\"}"}
```

## 깊게 파헤치기

Clojure에서 HTTP 요청을 보내는 방법에 대해 더 자세히 알아보겠습니다. 클라이언트 라이브러리는 많은 다양한 옵션을 제공합니다. 기본적인 신택스 외에도 여러 옵션을 사용하여 요청을 보낼 수 있으며, 이를 통해 다양한 유형의 요청을 보낼 수 있습니다. 예를 들어, :headers 옵션을 통해 헤더를 추가하여 특정 정보를 전달할 수 있습니다.

```Clojure
(client/request {:method :get :url "https://www.example.com" :headers {"User-Agent" "My-Clojure-App"}})
```

또한 임의의 복잡한 용도로 사용하기 위해, 클라이언트 라이브러리는 콜백 함수를 사용할 수 있는 옵션을 제공합니다. 이를 통해 요청을 보내고 받은 응답을 직접 처리할 수 있습니다.

```Clojure
(client/request {:method :get
                  :url "https://api.example.com/items"
                  :params {:limit 10}
                  :callback (fn [response] (println (count (:body response)))))})
```

더 많은 옵션과 사용 방법은 공식 문서를 참조하세요.

## 참고 자료

- [공식 Clojure 문서](https://clojure.org)
- [clj-http 라이브러리](https://github.com/dakrone/clj-http)
- [Clojure 공식 라이브러리 문서](https://clojure.github.io/clojure/clojure.core-api.html)