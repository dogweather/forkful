---
title:                "정규 표현식 사용하기"
html_title:           "PHP: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 사용할까요?

정규표현식을 사용하는 이유는 간단합니다. 우리가 특정한 패턴을 찾거나 대체하는 일이 필요할 때가 있기 때문입니다.

이를테면, 이메일 주소, 전화번호, 주민등록번호 등과 같은 특정한 형식을 가진 문자열을 찾아내거나, 특정한 단어를 대체하고 싶거나, 혹은 문자열에서 원하는 부분만 추출해내는 등의 작업을 할 때 정규표현식이 필요합니다.

## 사용 방법

정규표현식을 사용하기 위해선 먼저 PHP에서 제공하는 `preg_match()` 함수를 사용해야 합니다. 이 함수는 세 개의 매개변수를 가지고 있는데, 첫 번째 매개변수에는 찾아낼 패턴을 지정하고, 두 번째 매개변수에는 검사할 문자열을 지정하며, 세 번째 매개변수에는 옵션을 설정할 수 있습니다.

아래는 이메일 주소를 검사하는 예제 코드입니다.

```PHP
$email = "john.doe@email.com";

if (preg_match("/[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}/", $email)) {
  echo "유효한 이메일 주소입니다.";
} else {
  echo "유효하지 않은 이메일 주소입니다.";
}
```

위의 코드를 실행하면 "유효한 이메일 주소입니다."라는 메시지가 출력될 것입니다. 우리가 지정한 정규표현식은 이메일 주소의 형식을 검사하는 역할을 합니다. 이와 같은 방식으로 전화번호, 주민등록번호 등도 각각의 형식에 맞게 정규표현식을 작성하여 검사할 수 있습니다.

## 심층 탐구

정규표현식을 사용할 때 주의할 점이 있습니다. 바로 지나치게 복잡한 패턴을 사용하는 것입니다. 정규표현식은 강력한 도구지만, 지나치게 복잡하고 난해한 패턴을 작성하면 오히려 유지보수가 어려워질 수 있습니다. 따라서 가능한 간단하고 명확한 패턴을 작성하는 것이 좋습니다.

또한, 정규표현식을 작성할 때는 테스트를 철저히 수행하는 것이 중요합니다. 패턴을 작성할 때마다 테스트를 수행하여 원하는 결과를 얻을 수 있도록 해야 합니다. 이렇게 함으로써 중간에 발생하는 오류를 최소화할 수 있습니다.

## 더 알아보기

- [PHP 공식 문서: 정규표현식 사용하기](https://www.php.net/manual/en/function.preg-match.php)
- [정규표현식 체크리스트](https://www.debuggex.com/cheatsheet/regex/php)
- [정규표현식 연습 사이트: regex101](https://regex101.com)

## 참고자료

- [PHP: 검증과 필터링 정규표현식](https://www.php.net/manual/en/filter.filters.sanitize.php)
- [W3Schools: PHP 정규표현식 연습](https://www.w3schools.com/php/php_regex.asp)
- [정규표현식을 사용한 입력 유효성 검사](https://www.tutorialspoint.com/php/php_regular_expression.htm)