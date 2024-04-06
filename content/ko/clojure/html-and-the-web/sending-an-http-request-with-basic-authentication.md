---
date: 2024-01-20 18:01:38.746165-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) HTTP \uAE30\uBCF8 \uC778\
  \uC99D(Basic Authentication)\uC740 'RFC 7617'\uC5D0 \uC815\uC758\uB418\uC5B4 \uC788\
  \uC73C\uBA70, \uC6F9\uC5D0\uC11C \uAC00\uC7A5 \uAC04\uB2E8\uD558\uACE0 \uC624\uB798\
  \uB418\uC5C8\uC9C0\uB9CC \uAFB8\uC900\uD788 \uC0AC\uC6A9\uB418\uB294 \uC778\uC99D\
  \ \uBC29\uC2DD\uC785\uB2C8\uB2E4. Base64 \uC778\uCF54\uB529\uC758 \uC0AC\uC6A9\uC790\
  \ \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C HTTP \uD5E4\uB354\uC5D0 \uD3EC\
  \uD568\uC2DC\uCF1C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.133330-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) HTTP \uAE30\uBCF8 \uC778\uC99D\
  (Basic Authentication)\uC740 'RFC 7617'\uC5D0 \uC815\uC758\uB418\uC5B4 \uC788\uC73C\
  \uBA70, \uC6F9\uC5D0\uC11C \uAC00\uC7A5 \uAC04\uB2E8\uD558\uACE0 \uC624\uB798\uB418\
  \uC5C8\uC9C0\uB9CC \uAFB8\uC900\uD788 \uC0AC\uC6A9\uB418\uB294 \uC778\uC99D \uBC29\
  \uC2DD\uC785\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

## How to: (어떻게 하나요?)
```Clojure
(require '[clj-http.client :as client])

(let [credentials "username:password"
      encoded-credentials (-> credentials .getBytes "UTF-8" java.util.Base64/getEncoder .encodeToString)
      headers {"Authorization" (str "Basic " encoded-credentials)}]
  (client/get "http://protected-resource.com" {:headers headers}))
```

Sample output:

```Clojure
{:status 200
 :headers ...
 :body "..."}
```

## Deep Dive (깊이 이해하기)
HTTP 기본 인증(Basic Authentication)은 'RFC 7617'에 정의되어 있으며, 웹에서 가장 간단하고 오래되었지만 꾸준히 사용되는 인증 방식입니다. Base64 인코딩의 사용자 이름과 비밀번호를 HTTP 헤더에 포함시켜 보내는 방식이죠. 

클로저(Clojure) 커뮤니티에서는 `clj-http` 라이브러리를 주로 사용하여 HTTP 요청을 처리하는데, 이 라이브러리는 기본 인증을 비롯한 다양한 HTTP 기능을 추상화하여 제공합니다. `clj-http` 이외에도, `http-kit`이나 `aleph` 같은 다른 HTTP 라이브러리를 사용할 수도 있습니다.

이와 같은 라이브러리들은 내부적으로 Java의 `HttpURLConnection` 클래스나 Apache의 `HttpClient` 등을 사용하여 인증 프로세스를 간소화합니다. 중요한 것은, 기본 인증의 취약점을 알고 있어야 한다는 점입니다. 예를 들어, HTTPS를 사용하지 않으면, Base64 인코딩된 자격 증명이 네트워크 스니핑에 의해 드러날 수 있습니다.

## See Also (더 보기)
- [clj-http GitHub Repository](https://github.com/dakrone/clj-http)
- [RFC 7617: The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
