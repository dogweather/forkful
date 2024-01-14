---
title:                "PHP: 문자열을 소문자로 변환하는 방법"
simple_title:         "문자열을 소문자로 변환하는 방법"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 대해 알아보는 이유는, 개발 프로젝트에서 사용자의 입력이나 데이터베이스에서 가져온 데이터를 처리해야 할 때 올바르게 작동하기 위해서입니다.

## 어떻게

문자열을 소문자로 변환하는 것은 PHP에서 간단한 작업입니다. 우리는 먼저 문자열을 변수에 할당하고, 그 변수를 strtolower() 함수에 매개변수로 전달합니다. 그리고 나서 바뀐 변수를 출력하면 됩니다.

```PHP
$string = "Hello World";
$string = strtolower($string);
echo $string; // outputs "hello world"
```

## 깊게 파헤치기

PHP에서 문자열을 소문자로 바꾸는 함수는 strtolower() 외에도 여러 가지가 있습니다. 예를 들어, mb_strtolower() 함수는 멀티바이트 문자열도 소문자로 변환할 수 있습니다. 또는 strtoupper() 함수를 사용하면 대문자로 변환할 수도 있습니다. 이러한 함수를 적절하게 사용하여 프로젝트에서 필요한 작업을 수행할 수 있습니다.

## 참고하기

- PHP strtolower() 함수: https://www.php.net/manual/en/function.strtolower.php
- PHP mb_strtolower() 함수: https://www.php.net/manual/en/function.mb-strtolower.php
- PHP strtoupper() 함수: https://www.php.net/manual/en/function.strtoupper.php