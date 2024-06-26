---
date: 2024-01-20 18:00:49.211256-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C HTTP\
  \ \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC00\uC7A5 \uAE30\uBCF8\uC801\uC778 \uBC29\
  \uBC95\uC740 `curl` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uC544\uB798\uB294 GET \uC694\uCCAD\uC744 \uBCF4\uB0B4\
  \uB294 \uAC04\uB2E8\uD55C \uC608\uC81C\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.053709-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C HTTP \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uAC00\uC7A5 \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740\
  \ `curl` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (어떻게 하나요?)
PHP에서 HTTP 요청을 보내는 가장 기본적인 방법은 `curl` 라이브러리를 사용하는 것입니다. 아래는 GET 요청을 보내는 간단한 예제입니다:

```php
<?php
$url = "https://api.example.com/data";

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);

if ($response === false) {
    echo 'Curl error: ' . curl_error($ch);
} else {
    echo 'Operation completed without any errors';
}

curl_close($ch);
?>
```

예제의 출력 결과는 다음과 같습니다:

```
Operation completed without any errors
```

POST 요청을 보낼 때는 조금 다릅니다. POST 필드를 설정해야 합니다:

```php
<?php
$url = "https://api.example.com/submit";
$data = ['name' => 'John', 'age' => '28'];

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_POST, true);
curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($data));
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

## Deep Dive (심층 분석)
HTTP 요청을 보내는 기능은 1990년대 초반 웹의 초창기부터 계속 진화해왔습니다. 초기에는 PHP의 `fsockopen()`과 같은 함수를 사용하여 낮은 수준의 커넥션을 직접 관리해야 했습니다.

하지만, 편의성과 강력한 기능을 제공하는 `cURL`은 지금 PHP에서 HTTP 요청을 보내는 주류 방법입니다. `cURL`은 다양한 프로토콜 지원, 인증, HTTP POST, 쿠키 등 HTTP 통신을 위한 풍부한 옵션을 제공합니다.

또 다른 대안으로는 PHP의 `file_get_contents()`와 `stream_context_create()` 함수가 있지만, 이들은 `cURL`만큼 강력한 옵션을 제공하지 않을 수 있습니다.

중요한 점은, `cURL`에는 PHP 확장 기능을 활성화해야 사용할 수 있다는 점입니다. `php.ini` 파일에서 `extension=curl`을 활성화해주어야 합니다.

## See Also (참고 자료)
- PHP cURL의 공식 문서: [PHP: cURL - Manual](https://www.php.net/manual/en/book.curl.php)
- HTTP 요청 방법에 대한 더 많은 정보: [HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- `file_get_contents()`와 `stream_context_create()` 사용법: [PHP: file_get_contents - Manual](https://www.php.net/manual/en/function.file-get-contents.php), [PHP: stream_context_create - Manual](https://www.php.net/manual/en/function.stream-context-create.php)
