---
title:                "PHP: 문자열을 소문자로 변환하기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜?
문자열을 소문자로 변환하는 것은 PHP 프로그래밍을 할 때 매우 유용합니다. 대소문자를 구분해야하는 경우, 대소문자를 통일하고 검색 및 비교를 용이하게하기 위해 문자열을 모두 소문자로 변환하는 것이 좋습니다.

## 하는 법
```PHP
<?php 
$string = "HELLO WORLD";
echo strtolower($string);
?>
```
출력: hello world

```PHP
<?php 
$string = "This is a SAMPLE TEXT";
echo strtolower($string);
?>
```
출력: this is a sample text

## 깊이 파보기
PHP에서는 strtolower() 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이 함수는 문자열을 소문자로 바꾸는 데 매우 유용하며 대소문자를 구분하지 않는 검색 및 비교가 필요할 때 매우 유용합니다. 대소문자를 구분하지 않는 기능은 대부분의 경우 유저 이름 및 이메일과 같은 데이터를 처리하는 데 매우 중요합니다. 또한 부분적으로 소문자로 변환하려는 경우 strtolower() 함수 대신 lcfirst() 함수를 사용할 수도 있습니다.

## 관련 링크
- PHP 공식 문서로 가기: https://www.php.net/manual/en/function.strtolower.php
- strtolower() 함수 예제: https://www.w3schools.com/php/func_string_strtolower.asp 
- 대소문자 구분을 고려한 문자열 비교: https://www.php.net/manual/en/function.strcasecmp.php