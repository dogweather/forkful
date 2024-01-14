---
title:                "PHP: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜

웹 페이지를 다운로드하는 이유는 다양합니다. 예를 들면, 해당 웹 페이지에 특정한 정보를 얻기 위해서일 수 있고, 해당 웹 페이지를 크롤링하는 스크립트를 작성하기 위해서일 수도 있습니다. 또는 소스 코드나 이미지 파일 등을 다운로드하여 자신의 프로젝트에 사용하기 위해서일 수도 있습니다.

## 다운로드하는 방법

PHP를 사용하여 웹 페이지를 다운로드하는 방법은 간단합니다. 우선, `file_get_contents()` 함수를 사용하여 해당 URL의 HTML 코드를 문자열로 가져옵니다. 그리고 `file_put_contents()` 함수를 사용하여 가져온 HTML 코드를 원하는 파일에 저장합니다. 아래는 이 과정을 보여주는 코드 예시입니다. 

```PHP
<?php
$url = "https://www.example.com";
// 웹 페이지의 HTML 코드를 가져와서 $html 변수에 저장합니다.
$html = file_get_contents($url);
// $html 변수에 저장된 HTML 코드를 "example.html" 파일에 저장합니다.
file_put_contents("example.html", $html);
```

위 코드를 실행하면 현재 디렉토리에 "example.html" 파일이 생성되고, 해당 파일을 열어보면 `https://www.example.com` 웹 페이지의 HTML 코드가 저장되어 있는 것을 확인할 수 있습니다. 만약 PHP 버전이 5.6 이상이라면 위의 예시 코드는 `file_get_contents()` 함수 대신 `file()` 함수를 사용하여 더 간단하게 작성할 수 있습니다.

## 깊이 들어가기

PHP에서 웹 페이지를 다운로드하는 방법에 대해 더 깊이 들어가보겠습니다. 위에서 설명한 방법은 가장 간단하고 기본적인 방법으로, 간단한 스크립트를 작성할 때 유용하게 사용할 수 있습니다. 그러나 실제 프로젝트에서는 더 복잡한 웹 서버와의 통신을 위해 cURL 라이브러리를 사용하는 것이 좋습니다. 

또한, `file_put_contents()` 함수는 존재하는 파일을 덮어쓰기만 할 수 있기 때문에, 이미 파일이 존재하는 경우에는 이전 내용이 모두 사라지고 새로운 내용이 저장됩니다. 이를 방지하기 위해 `file_put_contents()` 대신 `file_get_contents()`를 사용할 수 있습니다. 

더 자세한 내용을 알고 싶다면 [PHP 공식 문서](https://www.php.net/manual/en/function.file-get-contents.php)를 참고하시기 바랍니다.

## 더 알아보기

- [PHP cURL 라이브러리 공식 문서](https://www.php.net/manual/en/book.curl.php)
- [cURL을 사용하여 웹 페이지 다운로드하기](https://www.tutorialrepublic.com/php-tutorial/php-curl.php)
- [PHP Simple HTML DOM Parser를 사용하여 웹 페이지 파싱하기](https://simplehtmldom.sourceforge.io/)