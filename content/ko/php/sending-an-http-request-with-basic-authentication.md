---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

HTTP 요청은 웹 서버와 정보를 주고받기 위한 방법입니다. 기본 인증(Basic Authentication)을 사용하여 HTTP 요청을 보내면, 요청하는 사람이 누구인지 확인하는 절차를 추가하여 보안을 강화할 수 있습니다. 이는 웹 자원에 대한 무분별한 접근을 방지하는 데 도움이 됩니다.

## 실행 방법:

PHP를 사용하여 기본 인증이 있는 HTTP 요청을 보내는 방법은 다음과 같습니다.

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("username:password")
    ]
]);
$response = file_get_contents('http://example.com', false, $context);
?>
```
이 코드를 실행하면, 웹서버는 인증 정보(여기서는 'username'과 'password')를 갖고 있는 HTTP 요청을 받게 됩니다.

## 특별히 알아두기:

사실상, 기본 인증은 저수준 보안 메커니즘으로, 아주 오래 전부터 사용해 왔습니다. 그러나 여전히 통신을 암호화하지 않으므로 사용자 이름과 비밀번호가 인터넷을 통해 노출될 수 있습니다.

더 나은 대안은 베어러 토큰 또는 OAuth 같은 보다 진보된 인증 방식입니다. 그 외에도, Guzzle이나 cURL과 같은 PHP 라이브러리를 사용하면 복잡성을 줄이고 보다 안전한 요청을 할 수 있습니다.

기본 인증의 구현은 매우 간단하게 이루어질 수 있습니다. 인증 정보를 base64 인코딩한 후 "Authorization: Basic" 헤더에 추가하기만 하면 됩니다.

## 참고자료:

2. [PHP cURL 공식 문서](https://www.php.net/manual/en/book.curl.php)
3. [Basic Authentication에 대한 모질라(Mozilla) 문서](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)
4. [OAuth에 대한 RFC 문서](https://tools.ietf.org/html/rfc6749)