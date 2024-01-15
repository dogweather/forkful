---
title:                "Http 요청 보내기"
html_title:           "PHP: Http 요청 보내기"
simple_title:         "Http 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP request를 보내는 것의 가장 큰 이유는 웹 서버와 통신하여 웹 페이지나 데이터를 얻기 위함입니다.

## 어떻게

```PHP
// HTTP request를 보내는 예제 코드
$request = curl_init(); // cURL 세션 초기화
curl_setopt($request, CURLOPT_URL, "https://www.example.com"); // 내가 원하는 URL로 설정
curl_setopt($request, CURLOPT_RETURNTRANSFER, 1); // cURL 함수에서 반환된 값을 직접 출력하지 않고 변수에 저장
curl_exec($request); // 설정된 옵션들을 사용하여 HTTP request를 보냄
curl_close($request); // cURL 세션 닫기
```

위 코드는 cURL 함수를 사용하여 HTTP request를 보내는 간단한 예제입니다. cURL은 PHP에서 HTTP 통신을 가능하게 해주는 라이브러리로, 많은 옵션을 제공하여 더욱 다양한 요청을 보낼 수 있습니다.

## 깊게 파헤치기

HTTP request를 보낼 때, GET과 POST 방식이 가장 많이 사용됩니다. GET 방식은 주소창을 통해 데이터를 전송하는 방식으로, 보안이 취약해 중요한 정보를 전달하지 않는 것이 좋습니다. 반면 POST 방식은 요청에 필요한 데이터를 HTTP body에 포함시켜 전송하는 방식으로, GET 방식보다 보안이 뛰어나고 많은 양의 데이터를 전송할 수 있습니다.

또한 HTTP request를 보낼 때, HTTP header를 지정하는 것도 중요합니다. header에는 요청의 다양한 정보를 포함시켜 서버에서 요청을 처리할 때 참고할 수 있도록 만들어줍니다.

## 더 알아보기

* [PHP cURL 공식 문서](https://www.php.net/manual/en/book.curl.php)
* [GET vs. POST 방식의 차이](https://www.diffen.com/difference/GET-vs-POST-HTTP-Requests)
* [HTTP header 정보](https://www.tutorialspoint.com/http/http_header.htm)

## 참고

이 글은 PHP 버전 7.4을 기준으로 작성되었습니다. 코드의 결과는 서버 환경에 따라 다를 수 있습니다.