---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜 & 왜?

HTTP 인증 요청을 보내는 것은 사용자 인증 정보를 포함한 HTTP 요청을 보내는 것을 말합니다. 이를 통해 프로그래머는 보안적으로 보호된 곳에 접근할 수 있습니다.

## 이곳에서:

Clojure에서 기본 인증을 사용하여 HTTP 요청을 보내는 가장 일반적인 방법은 `clj-http` 라이브러리를 사용하는 것입니다. 이 라이브러리를 사용하려면 먼저 프로젝트에 추가해야합니다.

```Clojure
(defproject yourproject "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR Apache-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.10.0"]])
```

이제 기본 인증을 사용하여 HTTP 요청을 보낼 수 있습니다.

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com"
                          {:basic-auth ["username" "password"]})]
  (println (:status response) (:body response)))
```

## 깊이있게 이해하기:

HTTP 인증은 웹의 초기 단계부터 있었으며, 사용자 이름과 비밀번호를 이용한 보안 방식을 제공하는 방법 중 하나입니다. 비록 오늘날 OAuth 같은 보다 강력한 방법이있지만, 기본 인증은 그 간결함과 적용의 용이성 때문에 여전히 사용됩니다.

Clojure에서는 `clj-http` 이외에도 `http-kit` 또는 `aleph` 같은 여러 라이브러리들이 제공되어 HTTP 요청을 처리할 수 있습니다. 선택은 당신의 특정 요구사항에 달려 있습니다.

## 관련자료:

- `clj-http` 라이브러리 사이트: https://github.com/dakrone/clj-http
- `http-kit` 라이브러리 사이트: http://http-kit.org/
- `aleph` 라이브러리 사이트: https://github.com/ztellman/aleph