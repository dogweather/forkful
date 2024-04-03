---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:04.940583-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB294\uAC00: PHP\uC5D0\uC11C JSON\uC744 \uB2E4\
  \uB8E8\uB294 \uAC83\uC740 \uB0B4\uC7A5 \uD568\uC218\uC778 `json_encode()` \uBC0F\
  \ `json_decode()` \uB355\uBD84\uC5D0 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uC544\uB798\
  \ \uC608\uC81C\uB294 PHP \uBC30\uC5F4\uC744 JSON \uBB38\uC790\uC5F4\uB85C \uBCC0\
  \uD658\uD558\uB294 \uBC29\uBC95\uACFC \uADF8 \uBC18\uB300\uC758 \uBC29\uBC95\uC744\
  \ \uBCF4\uC5EC\uC90D\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:55.388185-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uB0B4\uC7A5\
  \ \uD568\uC218\uC778 `json_encode()` \uBC0F `json_decode()` \uB355\uBD84\uC5D0 \uAC04\
  \uB2E8\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
