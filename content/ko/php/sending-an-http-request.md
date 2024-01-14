---
title:                "PHP: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜?

HTTP 요청을 보내는것은 웹 개발에서 중요한 부분입니다. 사용자의 입력을 받아서 검증하거나, 다른 서버에 있는 데이터를 받아오는 등 다양한 용도로 HTTP 요청을 사용할 수 있습니다.

## 어떻게?

```PHP
<?php
// GET 메서드로 HTTP 요청 보내기
$url = 'https://www.example.com';
$response = file_get_contents($url);

// POST 메서드로 HTTP 요청 보내기
$url = 'https://www.example.com/api';
$data = array('name' => 'John', 'age' => 25);
$options = array(
	'http' => array(
		'method' => 'POST',
		'header' => 'Content-Type: application/json',
		'content' => json_encode($data)
	)
);
$context = stream_context_create($options);
$response = file_get_contents($url, false, $context);
```

위의 예시 코드는 `file_get_contents()` 함수를 사용하여 간단하게 HTTP 요청을 보내는 방법을 보여줍니다. `GET` 메서드일 경우엔 요청을 보낼 URL을 매개변수로 전달하면 됩니다.

`POST` 메서드일 경우에는 요청할 URL뿐만 아니라 요청의 본문 데이터와 함께 보낼 옵션들을 매개변수로 전달해야 합니다. 위의 예시 코드에서는 `Content-Type` 헤더를 `application/json`으로 설정하고, 본문 데이터로 `name`과 `age` 변수를 가진 배열을 `json_encode()` 함수를 이용하여 JSON 형태로 변환하여 전달하고 있습니다.

`file_get_contents()` 함수도 아래에서 설명할 `stream_context_create()` 함수를 이용하여 컨텍스트를 생성할 수 있습니다. 이 컨텍스트에서 옵션을 지정하여 더욱 세부적인 요청을 보낼 수 있습니다.

## 딥 다이브

HTTP 요청은 더욱 복잡한 경우에도 사용할 수 있습니다. `cURL` 라이브러리를 이용하면 더욱 다양한 옵션과 기능을 제공받을 수 있습니다. 또한 `POST` 요청의 경우에는 `$_POST` 변수를 이용하여 서버로부터 전달받은 데이터를 쉽게 처리할 수 있습니다.

HTTP 요청의 종류나 세부 사항에 대해서는 인터넷에서 더 많은 정보를 찾아보실 수 있습니다.

## 참고

- [PHP: HTTP 요청 보내기](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP: cURL 라이브러리](https://www.php.net/manual/en/book.curl.php)
- [PHP: $_POST 변수](https://www.php.net/manual/en/reserved.variables.post.php)