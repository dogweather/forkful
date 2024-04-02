---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:04.940583-07:00
description: "JSON \uB610\uB294 JavaScript Object Notation\uC740 \uC0AC\uB78C\uC774\
  \ \uC77D\uACE0 \uC4F0\uAE30 \uC27D\uACE0, \uAE30\uACC4\uAC00 \uD30C\uC2F1\uD558\uACE0\
  \ \uC0DD\uC131\uD558\uAE30 \uC26C\uC6B4 \uACBD\uB7C9\uC758 \uB370\uC774\uD130 \uAD50\
  \uD658 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uADF8 \uAC04\uACB0\uD568\uACFC \uC5B8\uC5B4 \uB3C5\uB9BD\uC131\uC73C\uB85C \uC778\
  \uD574 \uC11C\uBC84\uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04 \uB370\
  \uC774\uD130\uB97C \uAD50\uD658\uD558\uAE30 \uC704\uD574 \uC885\uC885 JSON\uC744\
  \ \uC0AC\uC6A9\uD558\uBA70,\u2026"
lastmod: '2024-03-13T22:44:55.388185-06:00'
model: gpt-4-0125-preview
summary: "JSON \uB610\uB294 JavaScript Object Notation\uC740 \uC0AC\uB78C\uC774 \uC77D\
  \uACE0 \uC4F0\uAE30 \uC27D\uACE0, \uAE30\uACC4\uAC00 \uD30C\uC2F1\uD558\uACE0 \uC0DD\
  \uC131\uD558\uAE30 \uC26C\uC6B4 \uACBD\uB7C9\uC758 \uB370\uC774\uD130 \uAD50\uD658\
  \ \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uADF8\
  \ \uAC04\uACB0\uD568\uACFC \uC5B8\uC5B4 \uB3C5\uB9BD\uC131\uC73C\uB85C \uC778\uD574\
  \ \uC11C\uBC84\uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04 \uB370\uC774\
  \uD130\uB97C \uAD50\uD658\uD558\uAE30 \uC704\uD574 \uC885\uC885 JSON\uC744 \uC0AC\
  \uC6A9\uD558\uBA70,\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 무엇이며 왜인가?
JSON 또는 JavaScript Object Notation은 사람이 읽고 쓰기 쉽고, 기계가 파싱하고 생성하기 쉬운 경량의 데이터 교환 형식입니다. 프로그래머들은 그 간결함과 언어 독립성으로 인해 서버와 웹 애플리케이션 간 데이터를 교환하기 위해 종종 JSON을 사용하며, 이는 현대 웹 개발과 API에서 핵심적인 역할을 합니다.

## 어떻게 하는가:
PHP에서 JSON을 다루는 것은 내장 함수인 `json_encode()` 및 `json_decode()` 덕분에 간단합니다. 아래 예제는 PHP 배열을 JSON 문자열로 변환하는 방법과 그 반대의 방법을 보여줍니다:

### PHP 배열을 JSON 문자열로 인코딩하기
```php
// 연관 배열 정의하기
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// PHP 배열을 JSON 문자열로 변환하기
$jsonString = json_encode($data);

// JSON 문자열 출력하기
echo $jsonString;
```
**샘플 출력:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### JSON 문자열을 PHP 배열로 디코딩하기
```php
// JSON 문자열
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// JSON 문자열을 PHP 배열로 변환하기
$data = json_decode($jsonString, true);

// PHP 배열 출력하기
print_r($data);
```
**샘플 출력:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### 타사 라이브러리 사용하기: GuzzleHttp
복잡한 JSON 및 웹 요청 처리를 위해, 인기 있는 PHP 라이브러리 중 하나는 GuzzleHttp입니다. 이는 HTTP 요청을 단순화하고 JSON 데이터를 쉽게 다룰 수 있습니다.

**Composer를 통한 설치:**
```
composer require guzzlehttp/guzzle
```

**예제 요청:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// JSON을 반환하는 API로 요청 보내기
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// JSON 응답을 PHP 배열로 디코딩하기
$data = json_decode($response->getBody(), true);

// 데이터 출력하기
print_r($data);
```

**API가 비슷한 JSON 데이터를 반환한다고 가정할 때:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
이는 PHP를 사용하여 JSON을 조작하는 것이 기본 함수와 복잡한 작업을 위한 강력한 라이브러리인 GuzzleHttp와 함께 사용하기 쉬움을 보여줍니다.
