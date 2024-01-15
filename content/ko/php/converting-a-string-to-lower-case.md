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

## 왜
문자열을 소문자로 변환하는 이유는 대소문자 구분 없이 문자열을 처리하기 위해서입니다.

## 방법
```PHP
$string = "Hello World!";
echo "원본 문자열: " . $string . "\n";
echo "소문자로 변환: " . strtolower($string);
```

출력:
```
원본 문자열: Hello World!
소문자로 변환: hello world!
```

## 깊이 들어가보기
PHP에서는 문자열을 소문자로 변환하는 함수인 strtolower()를 제공합니다. 이 함수를 사용하면 모든 문자열을 소문자로 변경하여 문자열을 다루기 쉬워집니다.

그러나 주의해야 할 점이 있습니다. PHP는 기본적으로 영문 대소문자를 구분합니다. 따라서 문자열을 대소문자 구분 없이 처리해야 할 경우, strtolower() 함수 뿐만 아니라 mb_strtolower() 함수를 사용해야 합니다. 이 함수는 다국어 문자열에 대한 대소문자 구분 없는 변환을 지원합니다. 또한, 대문자를 소문자로 변환할 때에는 항상 대문자로 이루어진 문자열을 사용해야 합니다. 대문자가 아닌 문자가 포함되어 있을 경우에는 변환이 제대로 이루어지지 않을 수 있습니다.

## 관련 링크
- [PHP strtolower() 함수 문서](https://www.php.net/manual/kr/function.strtolower.php)
- [PHP mb_strtolower() 함수 문서](https://www.php.net/manual/kr/function.mb-strtolower.php)