---
title:                "PHP: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜?

HTTP 요청을 기본 인증과 함께 보내는 이유는 보안을 위한 것입니다. 기본 인증은 사용자 이름과 비밀번호를 인코딩하여 서버에 보내며, 이를 통해 사용자가 인증된 요청을 수행할 수 있습니다. 이는 웹 애플리케이션에서 중요한 정보를 안전하게 전송할 수 있도록 도와줍니다.

## 어떻게?

```PHP
<?php
// cURL 초기화
$ch = curl_init();

// 요청 URL 설정
curl_setopt($ch, CURLOPT_URL, 'www.example.com');

// 기본 인증 설정
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");

// 요청 전송
$result = curl_exec($ch);

// 결과 출력
echo $result;

// cURL 세션 닫기
curl_close($ch);
```

위의 코드는 PHP에서 cURL을 이용하여 기본 인증을 사용하여 HTTP 요청을 보내는 예제입니다. 먼저 cURL을 초기화하고 요청할 URL을 설정합니다. 그리고 `curl_setopt()` 함수를 사용하여 기본 인증을 설정하고, `curl_exec()` 함수를 통해 요청을 보냅니다. 마지막으로 결과를 출력하고 cURL 세션을 닫습니다.

## 깊게 들어가기

HTTP 요청에서 기본 인증은 보안을 위한 중요한 요소입니다. 보내는 데이터가 노출되지 않도록 하기 위해, 사용자 이름과 비밀번호를 Base64 인코딩하여 요청 헤더에 포함시킵니다. 서버는 요청 헤더를 읽고 로그인 정보를 확인한 후, 요청을 처리합니다. 사용자 이름과 비밀번호가 일치하지 않으면 인증 오류가 발생하며, 요청을 거부합니다.

## 또 다른 참고 자료

- [PHP cURL 공식 문서](https://www.php.net/manual/en/book.curl.php)
- [HTTP 요청 송신하기 (PHP)](https://www.w3schools.com/php/php_ajax_php.asp)
- [기본 인증 (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)