---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열 연결은 두 문자열을 합치는 과정을 의미합니다. 이는 프로그래머에게 문장이나 내용을 여러 문자열 조각에서 동적으로 생성하거나 재구성 할 수 있는 유연성을 제공하기 위함입니다.

## 어떻게:

PHP에서 문자열을 연결하려면 '`.`' 연산자를 사용합니다. 

```PHP
<?php
$greeting = "안녕하세요, ";
$name = "유저";
$message = $greeting . $name;
echo $message;
?>
```
위의 코드는 "안녕하세요, 유저"라는 메시지를 출력합니다.

## 심화 학습:

PHP에서 문자열 연결 규칙은 매우 직관적입니다. 보다 역사적인 관점에서, PHP는 버전 5.6부터 '.' 연산자를 이용한 문자열 연결을 지원하게 되었습니다. 

대안으로, PHP의 `sprintf` 또는 `printf` 함수를 사용하여 문자열에 변수를 삽입하고 연결할 수 있습니다. 

```PHP
<?php
$name = "유저";
$message = sprintf("안녕하세요, %s", $name);
echo $message;
?>
```
이 코드는 동일한 "안녕하세요, 유저" 메시지를 출력합니다.

`sprintf` 함수를 사용하면 문자열 연결이 필요 없는 대신 변수 이름을 문자열 내에 직접 포함시킬 수 있습니다.

## 참고 링크:

여기에 몇 가지 추가 자료를 제공하여 PHP의 문자열 처리에 대해 더 깊이 이해할 수 있게 하겠습니다.

1. PHP 공식 문서의 문자열 연결: https://www.php.net/manual/kr/language.operators.string.php
2. PHP 'sprintf' 함수에 대한 설명: https://www.php.net/manual/kr/function.sprintf.php
3. 문자열 처리를위한 다양한 PHP 기능에 대한 종합 가이드: https://www.php.net/manual/kr/book.strings.php