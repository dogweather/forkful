---
title:                "HTTP 요청 보내기"
html_title:           "PHP: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것, 그리고 이를 하는 이유는 무엇인지에 대해 알아보겠습니다. HTTP 요청은 클라이언트와 서버 사이에서 데이터를 교환하는 프로토콜입니다. 프로그래머들은 이를 사용하여 웹 페이지를 불러오거나, 데이터를 전송하고, API를 호출하는 등 다양한 작업을 할 수 있습니다.

## 방법:

다음은 PHP를 사용하여 HTTP 요청을 보내는 간단한 예시입니다. 먼저, `file_get_contents()` 함수를 사용하여 원하는 URL의 HTML 코드를 가져옵니다. 그 다음, `echo`를 사용하여 이를 출력합니다.

```PHP
<?php
    $url = "https://example.com";
    $html = file_get_contents($url);
    echo $html;
?>
```

위 코드를 실행하면, 해당 URL의 HTML 코드가 출력될 것입니다.

## 깊게 들어가기:

HTTP 요청은 World Wide Web의 탄생과 함께 도입된 프로토콜입니다. 이를 통해 웹 브라우저와 웹 서버가 데이터를 교환할 수 있게 되었습니다. 다른 대안으로는 cURL이나 Guzzle과 같은 라이브러리를 사용하여 HTTP 요청을 보낼 수 있습니다. PHP에서는 `curl_init()` 함수를 사용하여 cURL을 초기화하고, `curl_exec()` 함수를 사용하여 요청을 실행할 수 있습니다. 또한, `file_get_contents()` 함수는 URL을 포함한 다양한 매개변수를 받아 HTTP 요청을 보낼 수 있습니다.

## 관련 자료:

- [file_get_contents() 공식 문서](https://www.php.net/manual/en/function.file-get-contents.php)
- [cURL 공식 문서](https://www.php.net/manual/en/book.curl.php)
- [Guzzle 공식 문서](https://docs.guzzlephp.org/en/stable/)