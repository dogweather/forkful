---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청은 서버에 정보를 요청하거나 전송하는 방법입니다.프로그래머는 API 데이터 교환, 웹스크래핑 등을 위해 이것을 주로 사용합니다.

## 어떻게:
PHP에서 HTTP 요청을 보내는 대표적인 방법은 cURL을 사용하는 것입니다.

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL,"http://your-api-url.com/");
curl_setopt($ch, CURLOPT_POST, 1);
curl_setopt($ch, CURLOPT_POSTFIELDS, "param1=value1&param2=value2");

$result = curl_exec($ch);
curl_close($ch);
?>
```
$ch 변수에 cURL 세션을 초기화하고 URL, POST 메소드를 설정한 뒤, 필요한 필드값을 설정합니다. 마지막으로 curl_exec를 통해 요청을 보내고 결과를 $result 변수에 저장합니다.

## 깊게 이해하기
HTTP 요청의 기본 이해를 위해서는 웹의 역사적 맥락을 알 필요가 있습니다.
1990년대 초에 월드 와이드 웹이 등장하며, 클라이언트와 서버 사이의 통신 방식으로 HTTP가 도입되었습니다.

PHP에서 대안으로 사용할 수 있는 것은 `file_get_contents()` 함수와 `fopen()` 함수가 있습니다. 하지만 보안 이슈와 에러처리, 데이터 포맷팅 등의 문제로 cURL이 더 흔히 사용되며, HTTP 요청 전송에 가장 뛰어난 성능을 보여줍니다.

## 참고자료
- PHP cURL 공식 문서: [http://php.net/manual/en/book.curl.php](http://php.net/manual/en/book.curl.php)
- PHP file_get_contents() 함수: [http://php.net/manual/en/function.file-get-contents.php](http://php.net/manual/en/function.file-get-contents.php)
- PHP fopen() 함수: [http://php.net/manual/en/function.fopen.php](http://php.net/manual/en/function.fopen.php)

HTTP 요청은 웹 프로그래밍에서 필수적인 요소로, PHP에는 이를 지원하는 여러 가지 도구와 방법이 있습니다. 상황에 따라 가장 잘 맞는 방법을 선택하면 원활한 커뮤니케이션을 보장할 수 있습니다.