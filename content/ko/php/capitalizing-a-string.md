---
title:    "PHP: 스트링 대문자화하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것에 참여하는 이유는 그것이 쉽고 간단한 방법이라서입니다.

## 방법

우리는 `strtoupper()` 함수를 사용하여 문자열을 대문자로 바꿀 수 있습니다.

```PHP
<?php
$string = "안녕하세요";
echo strtoupper($string);
```

결과:

```
안녕하세요
```

위와 같이 함수를 사용하면 간단하게 문자열을 대문자로 바꿀 수 있습니다.

## 심층 분석

`strtoupper()` 함수는 PHP 내장 함수로, 문자열을 모두 대문자로 바꿔줍니다. 이 함수는 인자로 만들어진 문자열을 받으며, 반환 값으로 대문자로 변환된 문자열을 리턴합니다.

또한, 이 함수는 UTF-8 문자열을 지원하므로 언어에 관계없이 모든 문자를 대문자로 변환할 수 있습니다.

이 함수는 대소문자를 구분하지 않고 문자를 비교하기 때문에, 인자로 들어온 문자열이 이미 대문자로 되어 있어도 결과는 동일할 것입니다.

`strtoupper()` 함수를 이용해서 간단하게 문자열을 대문자로 바꿀 수 있지만, 자세히 보면 모든 문자를 대문자로 변환하기 때문에 적절한 상황에서만 사용하시기를 권장합니다.

## 더 알아보기

- PHP 공식 문서: http://php.net/manual/en/function.strtoupper.php
- PHP 문자열 변환 함수 비교: https://www.fldtrace.com/php/08-string-functions.html
- PHP 대문자 변환 함수 비교: https://www.techiedelight.com/php-uppercase-functions/