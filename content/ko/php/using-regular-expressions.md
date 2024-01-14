---
title:                "PHP: 정규식 사용하기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

##왜 Regular Expressions를 사용해야 하는가? 
정규표현식은 파이썬 프로그래밍에서 필수적인 요소입니다. 여러분은 이를 사용하여 문자열에서 원하는 패턴을 찾거나, 특정 문자열의 포맷을 검증하거나, 특정 문자열을 교체할 수 있습니다.

##어떻게 사용하나요? 
Regular Expressions는 다양한 방법으로 사용할 수 있지만, 가장 기본적인 방법은 "preg_match()" 함수를 사용하는 것입니다. 예를 들어, 다음과 같은 코드를 사용하여 이메일 주소의 유효성을 검증할 수 있습니다.

```PHP
$email = "example@example.com";
if (preg_match("/^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$/i", $email)) {
  echo "유효한 이메일 주소입니다.";
}
```
위 코드에서는 정규표현식을 사용하여 이메일 주소의 패턴을 검증하고 있습니다. "preg_match()" 함수는 해당 문자열에서 정규표현식 패턴과 일치하는 부분을 찾아내어, 찾지 못하면 "false"를 반환합니다.

##더 깊이 알아보기 
패턴 내용이 복잡해지면, 정규표현식 관련 메타문자를 사용하여 더 복잡한 패턴을 정의할 수 있습니다. 또한, "preg_match_all()" 함수를 사용하면 해당 문자열에서 일치하는 모든 패턴을 찾아낼 수도 있습니다.

See Also (더 알아보기) 
- PHP Manual: 정규표현식 (https://www.php.net/manual/kr/ref.pcre.php)
- 정규표현식 테스트 사이트 (https://regexr.com/)
- 정규표현식 관련 온라인 강의 (https://www.inflearn.com/course/%EC%A0%95%EA%B7%9C%ED%91%9C%ED%98%84%EC%8B%9D-regex/dashboard)