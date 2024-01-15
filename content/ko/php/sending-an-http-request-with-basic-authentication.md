---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "PHP: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청에 기본 인증을 사용하는 이유는 해당 요청에 보안을 추가하여 민감한 정보의 전송을 안전하게 보호하기 위해서입니다.

## 방법

PHP로 HTTP 요청을 보낼 때 기본 인증을 사용하는 방법은 간단합니다. 먼저, 요청을 보내려는 URL을 지정하고 `curl_init()` 함수를 사용하여 cURL 세션을 초기화합니다. 그런 다음 `curl_setopt()` 함수를 사용하여 `CURLOPT_HTTPAUTH` 및 `CURLOPT_USERPWD` 옵션을 설정하여 기본 인증을 사용하도록 설정합니다. 아래의 예제 코드와 출력을 참고하세요.

```PHP
<?php

// 요청을 보낼 URL
$url = "https://example.com/api";

// cURL 세션 초기화
$ch = curl_init();

// 기본 인증 사용 설정
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");

// URL 설정
curl_setopt($ch, CURLOPT_URL, $url);

// 요청 보내기
curl_exec($ch);

// 세션 닫기
curl_close($ch);

?>
```

**출력:**

```
HTTP/1.1 200 OK
Content-Type: application/json

{
    "message": "Hello, world!"
}
```

## 깊이 파고들기

기본 인증은 HTTP 요청에 추가적인 보안을 제공합니다. 이를 사용하여 인증 정보를 암호화하여 전송하고 서버에서는 해당 정보를 확인하여 요청을 인증할 수 있습니다. 또한 기본 인증은 보안 레벨이 낮기 때문에 민감한 정보를 전송할 때는 다른 인증 방식을 고려해야 합니다.

## 더 알아보기

[PHP cURL 공식 문서](https://www.php.net/manual/en/book.curl.php)

[cURL로 HTTP 요청 보내기](https://www.php.net/manual/en/curl.examples.php)

## 참고 자료

[cURL을 이용한 기본 인증 방식으로 HTTP 요청 보내기](https://medium.com/@zackzeele/curl-serve-request-easily-653eb285a6c6)

[PHP에서 기본 인증 사용하기](https://www.geeksforgeeks.org/how-to-use-basic-authentication-in-php/)