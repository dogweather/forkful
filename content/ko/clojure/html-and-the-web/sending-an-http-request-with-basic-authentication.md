---
date: 2024-01-20 18:01:38.746165-07:00
description: "HTTP \uC694\uCCAD\uC744 \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8\
  \ \uBCF4\uB0B4\uAE30\uB294 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\
  \uD638\uB85C \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD560 \uB54C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAC1C\
  \uC778 \uC815\uBCF4\uB97C \uC548\uC804\uD558\uAC8C \uC804\uC1A1\uD558\uACE0 \uAC80\
  \uC99D\uB41C \uC0AC\uC6A9\uC790\uB9CC\uC774 \uC811\uADFC\uD560 \uC218 \uC788\uB3C4\
  \uB85D \uD558\uAE30 \uC704\uD574 \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.660043-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC744 \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8 \uBCF4\
  \uB0B4\uAE30\uB294 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\
  \uB85C \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD560 \uB54C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAC1C\uC778\
  \ \uC815\uBCF4\uB97C \uC548\uC804\uD558\uAC8C \uC804\uC1A1\uD558\uACE0 \uAC80\uC99D\
  \uB41C \uC0AC\uC6A9\uC790\uB9CC\uC774 \uC811\uADFC\uD560 \uC218 \uC788\uB3C4\uB85D\
  \ \uD558\uAE30 \uC704\uD574 \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTTP 요청을 기본 인증과 함께 보내기는 사용자 이름과 비밀번호로 보호된 리소스에 접근할 때 사용합니다. 프로그래머들은 개인 정보를 안전하게 전송하고 검증된 사용자만이 접근할 수 있도록 하기 위해 이 방법을 사용합니다.

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
