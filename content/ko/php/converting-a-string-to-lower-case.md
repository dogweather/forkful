---
title:                "문자열을 소문자로 변환하기"
html_title:           "PHP: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

 문자열을 소문자로 변환하는 것은, 단순히 문자열의 모든 대문자를 소문자로 바꾸는 작업을 의미합니다. 프로그래머들은 이를 하는 이유는 일관성 있는 데이터 처리를 위해서입니다.

## 방법:

### 예제 1:
```PHP
$str = "Hello WORLD";
echo strtolower($str);
```
출력: "hello world"

### 예제 2:
```PHP
$str = "PHP is awesome";
echo strtolower($str);
```
출력: "php is awesome"

## 딥 다이브:

### 역사적 맥락:
소문자 변환 함수는 오래된 언어에서도 사용되었으며, 지금은 PHP 뿐만 아니라 거의 모든 프로그래밍 언어에서 기본 제공됩니다.

### 대안:
문자열을 소문자로 변환하는 또 다른 방법은 `mb_strtolower()` 함수를 사용하는 것입니다. 이 함수는 다국어 문자열에서도 작동하며, 대소문자 변환의 결과가 현재 시스템의 로캘에 따라 달라지는 문제를 해결할 수 있습니다.

## See Also:

- [PHP strtolower() function](https://www.php.net/manual/en/function.strtolower.php) - 공식 PHP 문서에서 소문자 변환 함수에 대한 자세한 설명을 확인할 수 있습니다.
- [PHP mb_strtolower() function](https://www.php.net/manual/en/function.mb-strtolower.php) - 다국어 문자열에 대한 소문자 변환 함수에 대한 자세한 설명을 확인할 수 있습니다.
- [Unicode and Case Folding](https://www.unicode.org/faq/casemap_charprop.html) - 유니코드에서 대소문자 변환에 대한 규칙을 자세히 알아볼 수 있습니다.