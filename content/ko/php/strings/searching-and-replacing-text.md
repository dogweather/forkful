---
date: 2024-01-20 17:58:20.543234-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PHP\uC5D0\uC11C \uBB38\
  \uC790\uC5F4 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 `str_replace` \uD568\uC218\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.335809-06:00'
model: gpt-4-1106-preview
summary: "PHP\uC5D0\uC11C \uBB38\uC790\uC5F4 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294\
  \ `str_replace` \uD568\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (어떻게 하나요?)
PHP에서 문자열 검색 및 교체는 `str_replace` 함수를 사용합니다. 살펴보겠습니다:

```PHP
<?php
$originalText = "안녕하세요, PHP를 사용하여 텍스트를 교체해봅니다.";
$search = "안녕하세요";
$replace = "반갑습니다";
$result = str_replace($search, $replace, $originalText);

echo $result; // "반갑습니다, PHP를 사용하여 텍스트를 교체해봅니다."
?>
```

`preg_replace`로 정규 표현식 검색도 가능합니다:

```PHP
<?php
$originalText = "PHP는 2023년 버전이 엄청나게 멋집니다.";
$pattern = "/[0-9]{4}/";
$replace = "20XX";
$result = preg_replace($pattern, $replace, $originalText);

echo $result; // "PHP는 20XX년 버전이 엄청나게 멋집니다."
?>
```

## Deep Dive (심층 분석)
`str_replace`는 PHP 4부터 사용되어 왔으며, `preg_replace`는 PHP 3부터 존재합니다. 성능 면에서 `str_replace`가 간단한 문자열 교체에 더 빠르나, 복잡한 패턴이 필요할 땐 `preg_replace`가 유용합니다. `strtr` 함수나 `mb_ereg_replace()` 같은 다중 바이트 문자열 처리 함수도 있습니다.
`preg_replace_callback` 함수는 교체 로직에 사용자 정의 함수를 적용할 수 있게 해줍니다. 이는 복잡한 교체 패턴이나 문자열 가공에 유용합니다.

## See Also (추가 정보)
- PHP Official Documentation - String Functions: https://www.php.net/manual/en/ref.strings.php
- PHP Official Documentation - PCRE Functions: https://www.php.net/manual/en/ref.pcre.php
- Regular Expressions Info - PHP Regex: https://www.regular-expressions.info/php.html
- W3Schools PHP String Functions: https://www.w3schools.com/php/php_ref_string.asp
