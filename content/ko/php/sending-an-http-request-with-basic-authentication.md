---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:02:05.435471-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

category:             "PHP"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하나요?)
HTTP 요청 시 기본 인증을 사용하면 서버에 사용자 자격 증명을 전달합니다. 이 방법은 API 사이의 간단한 인증을 위해 프로그래머들이 사용합니다.

## How to: (어떻게 하나요?)
PHP로 기본 인증을 포함한 HTTP 요청을 보내는 예제입니다.

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'user1';
$password = 'pass123';

$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$result = file_get_contents($url, false, $context);

if ($result !== false) {
    echo '성공적으로 데이터를 받았습니다:';
    var_dump($result);
} else {
    echo '요청이 실패했습니다.';
}
?>
```

Sample Output:
```
성공적으로 데이터를 받았습니다:
string(23) "{"data": "sample data"}"
```

## Deep Dive (깊이 알아보기)
기본 인증(Basic Authentication)은 HTTP 1.0 때부터 사용되었습니다. 'Authorization' 헤더에 사용자 이름과 비밀번호를 `base64`로 인코딩하여 보냅니다. 보안 강화를 위해서는 HTTPS를 사용하세요. 대안으로 OAuth 같은 토큰 기반 인증이 있습니다. 실제 구현할 때 `cURL`이나 `Guzzle` 같은 HTTP 클라이언트 라이브러리를 사용하는 것이 더 나을 수 있습니다.

## See Also (관련 자료)
- [PHP: HTTP context options](https://www.php.net/manual/en/context.http.php)
- [PHP: Basic Authentication](https://www.php.net/manual/en/features.http-auth.php)
- [Guzzle, PHP HTTP client](http://docs.guzzlephp.org/en/stable/)
- [cURL in PHP](https://www.php.net/manual/en/book.curl.php)
