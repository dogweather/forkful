---
date: 2024-01-20 17:44:28.217198-07:00
description: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uC778\uD130\uB137 \uC0C1\uC758 \uD398\uC774\uC9C0\uB97C \uAC00\
  \uC838\uC640\uC11C \uC11C\uBC84\uB098 \uB85C\uCEEC \uCEF4\uD4E8\uD130\uC5D0 \uC800\
  \uC7A5\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uB370\uC774\uD0C0 \uBD84\uC11D, \uC2A4\uD06C\uB798\uD551, \uB610\uB294 \uBC31\
  \uC5C5\uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.277474-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC778\uD130\uB137 \uC0C1\uC758 \uD398\uC774\uC9C0\uB97C \uAC00\uC838\
  \uC640\uC11C \uC11C\uBC84\uB098 \uB85C\uCEEC \uCEF4\uD4E8\uD130\uC5D0 \uC800\uC7A5\
  \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\
  \uC774\uD0C0 \uBD84\uC11D, \uC2A4\uD06C\uB798\uD551, \uB610\uB294 \uBC31\uC5C5\uC744\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
웹 페이지를 다운로드한다는 것은 인터넷 상의 페이지를 가져와서 서버나 로컬 컴퓨터에 저장하는 것입니다. 프로그래머는 데이타 분석, 스크래핑, 또는 백업을 위해 이 작업을 수행합니다.

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
