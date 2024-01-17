---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "PHP: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 패턴과 일치하는 문자를 제거하는 것은 프로그래머들이 하는 것입니다. 이를하는 이유는 보안, 데이터 정리 또는 데이터를 더 쉽게 처리하기 위해서입니다.

## 방법:
```php
// 문자열에서 숫자만 남기는 예제
$string = "Hello123";
$output = preg_replace("/[^0-9]/", "", $string);
echo $output; // 123
```

```php
// 이메일 주소에서 도메인만 남기는 예제
$email = "example@gmail.com";
$output = preg_replace("/[^@]+$/", "", $email);
echo $output; // gmail.com
```

## 깊이 파헤치기:
(1) 삭제하는 문자열 패턴과 관련된 역사적 맥락, (2) 대체 방법 및 (3) 구현 세부 정보 등과 같은 깊은 내용을 살펴볼 수 있습니다.

## 또 다른 참고자료:
문자열에서 패턴과 일치하는 문자를 제거하는 방법에 대한 자세한 정보는 다음 링크를 참조하세요.
- https://www.php.net/manual/en/function.preg-replace.php
- https://www.w3schools.com/php7/php7_preg_replace.asp