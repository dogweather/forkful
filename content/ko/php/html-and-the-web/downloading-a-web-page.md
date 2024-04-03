---
date: 2024-01-20 17:44:28.217198-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C\uB294\
  \ `file_get_contents()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD574 \uC190\uC27D\uAC8C\
  \ \uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.355337-06:00'
model: gpt-4-1106-preview
summary: "PHP\uC5D0\uC11C\uB294 `file_get_contents()` \uD568\uC218\uB97C \uC0AC\uC6A9\
  \uD574 \uC190\uC27D\uAC8C \uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## How to: (어떻게 하나요?)
PHP에서는 `file_get_contents()` 함수를 사용해 손쉽게 웹 페이지를 다운로드할 수 있습니다.

```PHP
<?php
$url = "http://example.com";
$pageContent = file_get_contents($url);

if ($pageContent !== false) {
    // 페이지 내용이 성공적으로 로드됐을 경우,
    // $pageContent 변수를 사용해 작업하세요.
    echo "Page downloaded!";
    file_put_contents('downloaded_page.html', $pageContent);
} else {
    // 페이지 로드에 실패했습니다.
    echo "Failed to download the page.";
}
?>
```
샘플 출력:
```
Page downloaded!
```

## Deep Dive (심층 분석)
과거에 웹 페이지를 다운로드하는 다른 방법들도 있었습니다. 예를 들면, `cURL` 라이브러리를 사용하는 방법이 있죠. `cURL`은 보다 복잡한 작업에 적합하며, HTTP 메소드, 헤더, 쿠키 등을 커스터마이즈할 수 있는 이점이 있습니다. 반면에, `file_get_contents()`는 간단한 GET 요청에 이상적입니다. 하지만, 웹 서버 설정에 `allow_url_fopen`이 활성화되어 있어야 하며, 사용할 때 리모트 서버의 환경 또는 네트워크 문제로 인한 에러 가능성을 고려해야 합니다. 버전에 따라 HTTPS 프로토콜을 사용하기 위해서는 OpenSSL 확장이 활성화되어 있는지 확인해야 합니다.

## See Also (함께 보기)
- PHP Manual on file_get_contents: https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual on cURL: https://www.php.net/manual/en/book.curl.php
- PHP The Right Way (Guidelines for Downloading pages): https://phptherightway.com
