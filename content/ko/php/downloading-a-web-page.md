---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?
웹 페이지를 다운로드하는 것은 인터넷에서 웹 페이지의 데이터를 로컬로 복사하여 가져오는 것입니다. 프로그래머들은 이를 통해 웹 사이트의 콘텐츠를 분석하고, 어플리케이션을 위한 데이타를 수집하거나, 오프라인에서 웹 페이지를 사용하기 위해 이를 수행합니다.

## 실행 방법:
Web 페이지를 다운로드하는 가장 간단한 방법은 `file_get_contents()` 함수를 사용하는 것입니다. 이를 통해 웹 페이지 콘텐츠를 모두 가져올 수 있습니다.

```PHP
<?php
  $url = 'https://www.example.com';
  $content = file_get_contents($url);
  echo $content;
?>
```

위 코드를 실행하면, 'https://www.example.com' URL에 해당하는 웹 페이지의 HTML 컨텐츠가 출력됩니다.

## 깊게 파보기:
웹 페이지 다운로드는 웹 페이지의 HTML 소스를 가져오는 과정으로, 인터넷 초기부터 존재했습니다. 하지만 이를 수행하는 방법은 여러가지가 있습니다. `file_get_contents()` 뿐만 아니라, cURL 라이브러리를 사용한 방법도 있습니다. cURL은 다양한 프로토콜을 지원하고, 복잡한 웹 페이지 다운로드 요구를 처리할 수 있다는 장점을 가지고 있습니다. `file_get_contents()`는 간단한 용도로 충분하지만, 상세한 설정이 필요한 경우 cURL이 더 좋은 선택일 수 있습니다.

## 참고 자료:
1. PHP file_get_contents() 함수: http://php.net/manual/kr/function.file-get-contents.php
2. cURL를 이용한 웹 스크래핑: https://www.php.net/manual/en/book.curl.php