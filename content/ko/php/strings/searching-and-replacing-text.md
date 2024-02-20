---
date: 2024-01-20 17:58:20.543234-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uAD50\uCCB4\uB294 \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uD2B9\uC815 \uD14D\uC2A4\uD2B8\uB97C \uCC3E\uC544 \uB2E4\uB978\
  \ \uD14D\uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uD615\uC2DD \uC870\
  \uC815, \uC790\uB3D9\uD654 \uB4F1\uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.247852
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uAD50\uCCB4\uB294 \uBB38\uC790\uC5F4\
  \uC5D0\uC11C \uD2B9\uC815 \uD14D\uC2A4\uD2B8\uB97C \uCC3E\uC544 \uB2E4\uB978 \uD14D\
  \uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uD615\uC2DD \uC870\uC815\
  , \uC790\uB3D9\uD654 \uB4F1\uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색과 교체는 문자열에서 특정 텍스트를 찾아 다른 텍스트로 바꾸는 것입니다. 프로그래머들은 데이터 정제, 형식 조정, 자동화 등을 위해 이 기능을 사용합니다.

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
