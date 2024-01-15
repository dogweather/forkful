---
title:                "문자열 대문자로 변환하기"
html_title:           "PHP: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 바꾸는 것의 이유는 서로 다른 형식의 문자열을 통합하거나 사용자가 입력한 문자열을 통일된 형식으로 저장하기 위해서입니다.

## 하는 방법
문자열을 대문자로 바꾸는 가장 간단한 방법은 `strtoupper()` 함수를 사용하는 것입니다. 아래는 코드 예제와 함께 출력 결과도 함께 제공됩니다.

```PHP
<?php
$name = "john";
echo strtoupper($name); // JOHN
```

또 다른 방법은 `ucwords()` 함수를 사용하는 것인데, 이 함수는 각 단어의 첫 글자를 대문자로 바꿔줍니다. 아래는 코드 예제와 함께 출력 결과도 함께 제공됩니다.

```PHP
<?php
$name = "john doe";
echo ucwords($name); // John Doe
```

## 깊이 파헤치기
PHP는 문자열을 다루는 다양한 내장 함수를 제공합니다. 이러한 함수를 사용하면 보다 정교하게 문자열을 바꿀 수 있습니다. 예를들어, `mb_strtoupper()` 함수는 멀티바이트 문자열도 대문자로 바꿔주는데, `strtoupper()` 함수는 ASCII 문자열만 변환할 수 있습니다. 또한 `str_replace()` 함수를 사용하면 특정 문자나 문자열을 다른 문자나 문자열로 바꿀 수도 있습니다. 이 함수를 사용하면 여러 개의 문자열을 동시에 바꿀 수도 있습니다.

## 관련 자료 보기
[PHP 공식 문서](https://www.php.net/manual/en/function.strtoupper.php)  
[PHP 문자열 관련 함수들](https://www.php.net/manual/en/ref.strings.php)