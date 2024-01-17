---
title:                "문자열의 길이 찾기"
html_title:           "PHP: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 길이를 찾는 것은 프로그래머에게 중요한 작업입니다. 문자열의 길이를 알아야만 적절한 조건문을 작성하거나 문자열을 자르는 등의 작업을 할 수 있습니다.

## 방법:
```
<?php 
$str = "안녕하세요";
// 문자열의 길이를 찾는 방법은 strlen() 함수를 사용합니다.
echo strlen($str); // 출력결과: 5
?>
```

## 깊이 있는 내용:
(1)  strlen() 함수는 PHP 4 버전에서 처음 도입되었습니다. (2) 다른 언어에서는 문자열의 길이를 구하는 다른 방법들도 제공되지만, PHP에서는 strlen() 함수가 가장 간단하고 효율적인 방법입니다. (3) PHP에서 문자열의 길이를 구하는 방법에는 mb_strlen() 함수를 사용할 수도 있습니다. 이 함수는 멀티바이트 문자열도 정확하게 카운트할 수 있습니다.

## 관련 자료:
- PHP 문자열 함수 문서: http://php.net/manual/kr/ref.strings.php
- strlen() 함수 문서: http://php.net/manual/kr/function.strlen.php