---
title:                "문자열 합치기"
html_title:           "PHP: 문자열 합치기"
simple_title:         "문자열 합치기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜하는 거죠?

문자열 연결(concatenation)이란 여러 개의 문자열을 하나로 합치는 작업을 말합니다. 이는 문자열을 조작하거나 출력할 때 특히 유용합니다. 예를 들어, 여러 개의 이메일 주소를 하나의 문자열로 만들어서 한 번에 전송하거나, 긴 HTML 코드를 하나의 문자열로 만들어서 출력할 수 있습니다.

## 하는 방법은?

```PHP
// 문자열 연결 예제 (Concatenation Example)
$email1 = "example1@gmail.com";
$email2 = "example2@gmail.com";

// 문자열 연결 연산자를 사용해서 이메일 주소를 하나의 문자열로 합칩니다.
$email_string = $email1 . ", " . $email2;

// 합쳐진 문자열을 출력합니다.
echo $email_string;

// 출력 결과: example1@gmail.com, example2@gmail.com
```

## 깊숙히 알아보기

### 역사적 배경
PHP 언어에서의 문자열 연결은 이미 오래된 역사를 가지고 있습니다. 하지만 최근에는 신기술인 문자열 보간(string interpolation)이 보다 쉽고 간편한 방법으로 문자열 연결을 대체하고 있습니다.

### 대안들
문자열 연결에는 여러 가지 방법이 있습니다. 언어마다 다르지만, PHP에서는 더 편리한 문자열 보간 연산자인 `$`를 사용할 수도 있습니다. 또 다른 대안으로는 `sprintf` 함수를 사용하는 것이 있습니다.

### 세부적인 구현 방법
PHP에서는 문자열 연결 연산자인 `.`을 사용하면 됩니다. 이 연산자는 왼쪽에서 오른쪽으로 문자열을 합치는 역할을 합니다. 또한, 문자열을 합치는 과정에서 자동으로 문자열을 변환(conversion)하지 않기 때문에 더욱 효율적인 문자열 연결이 가능합니다.

## 관련 자료들

- [PHP 문서 - 문자열 연결(Concatenation)](https://www.php.net/manual/en/language.operators.string.php)
- [PHP 문서 - 문자열 보간(String Interpolation)](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)