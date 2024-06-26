---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:07.914379-07:00
description: "\uC5B4\uB5BB\uAC8C? PHP\uB294 \uB2E4\uC591\uD55C \uBAA9\uC801\uC744\
  \ \uC704\uD574 \uC11C\uBE44\uC2A4\uD558\uB294 \uC5EC\uB7EC \uBB38\uC790\uC5F4 \uB300\
  \uBB38\uC790\uD654 \uD568\uC218\uB97C \uB124\uC774\uD2F0\uBE0C\uB85C \uC9C0\uC6D0\
  \uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uADF8\uAC83\uB4E4\uC744 \uC0AC\uC6A9\uD558\
  \uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.333073-06:00'
model: gpt-4-0125-preview
summary: "PHP\uB294 \uB2E4\uC591\uD55C \uBAA9\uC801\uC744 \uC704\uD574 \uC11C\uBE44\
  \uC2A4\uD558\uB294 \uC5EC\uB7EC \uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654 \uD568\
  \uC218\uB97C \uB124\uC774\uD2F0\uBE0C\uB85C \uC9C0\uC6D0\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 어떻게?
PHP는 다양한 목적을 위해 서비스하는 여러 문자열 대문자화 함수를 네이티브로 지원합니다. 다음은 그것들을 사용하는 방법입니다:

### 문자열의 첫 글자를 대문자로 변경:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // 출력: Hello, world!
```

### 각 단어의 첫 글자를 대문자로 변경:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // 출력: Hello, World!
```

### 전체 문자열을 대문자로 변환:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // 출력: HELLO, WORLD!
```

더 맞춤화된 시나리오나 서드파티 솔루션을 요구할 때 `mbstring`(멀티바이트 문자열용)과 같은 라이브러리를 특히 기본 ASCII 세트를 넘어서는 문자를 다루는 국제화 작업에서 활용할 수 있습니다.

### mbstring을 사용하여 UTF-8 문자열의 첫 글자를 대문자로 변경:
PHP 구성에서 `mbstring` 확장 기능이 활성화되어 있는지 확인한 다음:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // 출력: Élégant
```

이 접근 방식은 ASCII가 아닌 문자를 포함하는 문자열을 정확하게 대문자화하는 데 도움이 되며, 다양한 언어의 미묘한 차이를 준수합니다.
