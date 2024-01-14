---
title:                "Clojure: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보낼 때 기본 인증을 사용하는 것의 이유는 해당 웹 사이트의 보안을 강화하고 사용자의 신원을 보호하기 위해서입니다.

## 사용 방법

```Clojure
(ns my-app.core
  (:require [clj-http.client :as http]))

(defn send-request []
  (http/post "https://example.com" 
             {:basic-auth ["username" "password"]}))
```

위의 예시 코드에서는 `clj-http` 라이브러리를 사용하여 `https://example.com` 에 `username` 과 `password`를 가진 사용자로부터 `POST` 요청을 보내는 방법을 보여줍니다.

다음은 요청의 결과를 확인하는 방법입니다.

```Clojure
{:status 200 
 :headers {"Content-Type" "text/plain"} 
 :body "Hello World!"}
```

위에서 언급한 것처럼 `clj-http` 라이브러리는 `basic-auth` 옵션을 헤더에 추가하여 기본 인증을 설정할 수 있습니다. 이를 통해 사용자의 유저네임과 패스워드를 포함한 인증 정보를 안전하게 전송할 수 있습니다.

## Deep Dive

기본 인증은 모든 HTTP 요청에서 사용할 수 있습니다. 그러나 이 방법은 보안상의 이유로 권장되는 방식은 아닙니다. 이 방식은 사용자의 정보가 요청의 서명에 포함되기 때문에 안전하지 않을 수 있습니다. 기본 인증을 사용할 경우, HTTPS 같은 추가적인 보안 계층을 사용하는 것이 좋습니다.

또한, 기본 인증은 인증 정보가 요청의 헤더에서 base64 인코딩됨을 알아야 합니다. 이는 실제로 안전하지 않기 때문에 인증 정보를 보호하기 위해서는 HTTPS를 사용하거나 다른 방법을 고려하는 것이 좋습니다.

## 볼거리

* [Clj-http 라이브러리 공식 문서](https://github.com/dakrone/clj-http) 
* [HTTP 기본 인증에 관한 자세한 정보](https://www.w3.org/Security/faq/wwwsf2.html#BRUTE-FORCE)