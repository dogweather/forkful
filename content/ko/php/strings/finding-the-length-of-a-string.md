---
title:                "문자열의 길이 찾기"
aliases:
- ko/php/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:56.974962-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜죠?)
문자열 길이 측정은 문자열에 있는 문자 수를 카운트합니다. 프로그래머가 데이터 처리, 입력 검증, 또는 UI 디자인을 조율할 때 이 정보가 필요합니다.

## How to: (어떻게 하죠?)
PHP에서 문자열의 길이를 찾으려면 `strlen()` 함수를 사용하세요. 간단하죠:

```php
<?php
$str = "안녕하세요";
echo strlen($str); // 출력: 15
?>
```
하지만 주의하세요, 이 예제의 출력은 문자 수가 아니라 바이트 수입니다. `mb_strlen()` 함수를 사용하면 더 정확한 문자 수를 얻을 수 있어요:

```php
<?php
$str = "안녕하세요";
echo mb_strlen($str, 'UTF-8'); // 출력: 5
?>
```

## Deep Dive (심층 분석)
`strlen()` 함수는 PHP 4.0.0부터 사용할 수 있습니다. 문자열의 바이트 수를 반환하지만, 멀티바이트 문자열(예: 한글)에 대해서는 잘못된 값을 줄 수 있습니다. 

`mb_strlen()` 함수는 멀티바이트 문자열에 대한 정확한 길이를 제공합니다. 이 함수는 `mbstring` 확장에 포함되어 있으며, 이 확장은 다국어 웹 애플리케이션을 지원하기 위해 고안되었습니다.

기억해야 할 또 다른 중요한 부분은 문자 인코딩입니다. `mb_strlen()`에서 올바른 길이를 얻으려면 사용하는 문자열의 인코딩을 정확히 명시해야 합니다. 

때로는 `utf8_decode()`를 사용하여 문자열을 ISO-8859-1로 변환한 다음 `strlen()`을 사용할 수도 있지만, 이 방법은 권장되지 않습니다. 왜냐하면 데이터 손실이 발생할 수 있기 때문입니다.

```php
<?php
$str = "안녕하세요";
echo strlen(utf8_decode($str)); // 사용하지 말 것
?>
```

## See Also (추가 정보)
- PHP Manual on `strlen()`: https://www.php.net/manual/en/function.strlen.php
- PHP Manual on `mb_strlen()`: https://www.php.net/manual/en/function.mb-strlen.php
- PHP Manual on strings: https://www.php.net/manual/en/language.types.string.php
- Understanding Character Encoding: https://www.smashingmagazine.com/2012/06/all-about-unicode-utf8-character-sets/
