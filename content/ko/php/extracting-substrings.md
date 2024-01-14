---
title:                "PHP: 문자열 추출하기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 추출하는 것의 이유는 여러 가지가 있을 수 있습니다. 가장 일반적인 이유는 주어진 문자열에서 원하는 부분만 골라내기 위해서입니다. 예를 들어, 사용자가 입력한 이메일 주소에서 도메인명만 추출하고 싶을 때, 문자열 추출을 사용할 수 있습니다.

## 방법
아래 코드 블록에서는 PHP를 사용하여 문자열 추출하는 방법을 보여줍니다. 첫 번째 예제는 substring 함수를 사용하는 간단한 예제이며, 두 번째 예제는 substr_replace 함수를 사용하여 문자열을 새로운 문자열로 바꾸는 예제입니다.
```PHP
<?php
// substring 함수 사용 예제
$email = "example@gmail.com";
$domain = substr($email, 8);
echo $domain; // 결과: gmail.com

// substr_replace 함수 사용 예제
$website = "www.example.com";
$new_website = substr_replace($website, "gmail.com", 4, -3);
echo $new_website; // 결과: www.gmail.com
```

## 깊이 들어가기
문자열 추출은 PHP에서 매우 유용한 기능입니다. substring 함수와 substr_replace 함수 외에도, 정규식 패턴을 사용하는 preg_match 함수를 사용하여 더 복잡한 문자열 추출도 가능합니다. 문자열 추출을 더 자세히 배우고 싶다면, PHP 공식 문서를 참조하는 것이 좋습니다.

## 참고 자료
- PHP 문자열 함수 (공식 문서): https://www.php.net/manual/kr/ref.strings.php
- PHP 정규식 패턴 (공식 문서): https://www.php.net/manual/kr/reference.pcre.pattern.syntax.php
- PHP와 정규식 패턴 강좌 (생활코딩): https://opentutorials.org/course/2598