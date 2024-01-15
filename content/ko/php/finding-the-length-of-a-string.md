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

## 왜
문자열의 길이를 찾는 작업에 참여하는 이유는 프로그램 또는 웹 애플리케이션에서 사용자가 입력한 데이터의 유효성을 확인하기 위해서입니다.

## 방법
문자열의 길이를 찾는 방법은 PHP의 내장 함수 중 하나인 `strlen()`을 사용하는 것입니다. `strlen()` 함수는 인자로 전달된 문자열의 길이를 정수로 반환합니다. 예를 들어, 다음과 같은 PHP 코드를 실행하면 문자열의 길이를 쉽게 찾을 수 있습니다.

```PHP
$name = "John Doe";
echo strlen($name); // Output: 8
```
`strlen()` 함수는 공백도 하나의 문자로 계산합니다. 따라서 "John Doe" 문자열의 길이는 8이 됩니다. 또한, 다른 변수나 상수의 값을 넣어서 문자열의 길이를 찾을 수도 있습니다.

```PHP
$website = "www.example.com";
echo strlen($website); // Output: 15
```

## 깊은 곳으로
`strlen()` 함수는 PHP 4부터 사용 가능한 내장 함수입니다. 이 함수는 다양한 문자열 데이터를 처리할 수 있으며, ASCII 문자, UTF-8, EUC-JP 등 다양한 문자 인코딩을 지원합니다. 또한, `mb_strlen()` 함수를 사용하면 멀티바이트 문자열을 처리할 수 있어 더욱 다양한 문자열 처리에 유용합니다. 

만약 문자열에서 공백을 제외하고 싶다면 `trim()` 함수를 사용하여 공백을 제거한 뒤 `strlen()` 함수를 적용할 수 있습니다. 또한, PHP 버전에 따라 `strlen()` 함수에서 문자열의 길이를 구하는 방식이 다를 수 있으니 문제가 발생하지 않도록 주의해야 합니다.

## 관련 자료
- PHP 공식 문서: [strlen() 함수](https://www.php.net/manual/en/function.strlen.php)
- W3Schools: [PHP strlen() 함수](https://www.w3schools.com/php/func_string_strlen.asp)
- TechOnTheNet: [PHP strlen() 함수](https://www.techonthenet.com/php/functions/strlen.php)