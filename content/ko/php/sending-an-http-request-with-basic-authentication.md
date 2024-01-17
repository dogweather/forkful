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

## HTTP 요청에 기본 인증을 사용하여 데이터를 전송하는 방법

HTTP 요청을 보내는 것은 PHP 프로그래머에게 중요한 기술입니다. 그 중에서도 기본 인증을 사용하여 요청을 보내는 것은 보안적인 이유로 더욱 중요합니다. 이 기사에서는 HTTP 요청에 기본 인증을 사용하는 방법에 대해 간단히 알아보겠습니다.

## 무엇 & 왜?

기본 인증은 HTTP 프로토콜의 한 형식으로서, 클라이언트가 서버로 데이터를 전송할 때 보안을 강화하기 위해 사용됩니다. 이는 전송되는 데이터를 암호화하여 무단 접근을 방지하고, 신분을 증명하여 누군지 확인할 수 있게 합니다. 프로그래머들은 이를 사용하여 송신되는 정보를 보호할 수 있기 때문에 기본 인증을 사용합니다.

## 사용 방법:

```php  
//기본 인증 정보를 설정  
$user = 'username';  
$password = 'password';  
$credentials = base64_encode($user . ':' . $password);  

//HTTP 헤더에 인증 정보 추가  
$options = [  
    'http' => [  
        'header' => "Authorization: Basic $credentials"  
    ]  
];  

//인증 정보를 포함한 요청 전송  
$url = 'https://www.example.com/api';  
$context = stream_context_create($options);  
$result = file_get_contents($url, false, $context);  

//결과 출력  
echo $result;  
```

## 더 깊게 알아보기:

1. 기본 인증은 HTTP 프로토콜의 일부로서 1996년에 소개되었습니다. 이후에도 여전히 많은 웹 서비스에서 사용되고 있습니다.
2. 기본 인증 외에도 여러가지 인증 방법이 존재하지만, 표준적인 인증 방법이 아니라서 보안에 취약할 수 있습니다.
3. PHP의 stream_context_create() 함수를 사용하여 인증 정보를 포함하는 컨텍스트를 생성할 수 있습니다.

## 관련 자료:

- https://www.php.net/manual/en/function.stream-context-create.php
- https://www.w3.org/Protocols/rfc2616/rfc2616-sec11.html
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication