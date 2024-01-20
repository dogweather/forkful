---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 문자열을 소문자로 변환하기

## 1. 무엇이며 왜 하는가?

문자열을 소문자로 변환하는 것은, 모든 대문자를 소문자로 변경하는 프로그래밍 동작을 의미합니다. 이는 대소문자를 구별하지 않는검색을 수행하거나, 사용자 입력을 정제해야 할 때 사용됩니다.

## 2. 어떻게 하는가?

PHP에서 문자열을 소문자로 변환하는 데는 `strtolower()` 함수를 사용합니다. 아래 예제를 참조하세요:

```PHP
<?php
$str = "Hello, World!";
$lowerCaseStr = strtolower($str);
echo $lowerCaseStr;
?>
```

이 스크립트를 실행하면 출력 결과는 "hello, world!"가 됩니다.

## 3. 깊이 파헤치기

* **역사적 맥락** : `strtolower()` 함수는 PHP가 처음 개발된 이후부터 존재하는 기본 함수입니다. PHP5에서는 이 함수가 이전 버전에서 발견된 몇 가지 버그를 수정하여 도입되었습니다.

* **대안** : `mb_strtolower()` 함수는 다국어 문자열에 작동하도록 구현된 `strtolower()`의 다국어 버전입니다.

* **구현 세부 사항** : `strtolower()` 함수는 문자열의 각 문자를 다루며 전방향 구문 분석을 수행합니다. 문자열을 전체적으로 메모리에 저장하고 대문자인 문자를 결정한 후 소문자로 변환합니다.

## 4. 참고 자료

* PHP 공식 문서 소문자 변환에 관한 정보 : [여기를 클릭하세요](https://www.php.net/manual/en/function.strtolower.php)
* mb_strtolower() 함수에 대한 추가 정보 : [여기를 클릭하세요](https://www.php.net/manual/en/function.mb-strtolower.php)
* 다국어 문자열 처리 방법에 대한 글 : [여기를 클릭하세요](https://www.php.net/manual/en/book.mbstring.php)