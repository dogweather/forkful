---
title:                "PHP: 정규 표현식 활용"
simple_title:         "정규 표현식 활용"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 할까요?

정규 표현식은 PHP 프로그래밍에서 빈번하게 사용되는 패턴 매칭 도구로, 문자열에서 특정 패턴을 찾아내거나 변환하는 데에 유용합니다. 이를 통해 좀 더 효율적이고 정확한 작업을 할 수 있게 됩니다.

# 어떻게 정규 표현식을 사용할 수 있을까요?

아래 코드 블록에서는 정규 표현식을 사용하여 이메일 주소에서 유저네임을 추출하는 예제를 보여드리겠습니다.

```PHP
$email = 'user123@example.com';
$username = preg_match('/^([a-z0-9]+)@([a-z]+\.[a-z]+)$/i', $email, $matches);

echo $matches[1]; // 출력 결과: user123
```

위 코드에서는 `preg_match()` 함수를 사용하여 정규 표현식과 문자열을 매칭시키고, `$matches` 배열에서 추출한 유저네임을 출력하였습니다. 이처럼 정규 표현식은 정확한 패턴을 지정하고 해당 패턴에 맞는 문자열을 찾아내는 데에 사용될 수 있습니다.

# 깊게 살펴보기

정규 표현식은 조금 복잡하게 느껴질 수 있지만, 많은 PHP 개발자들이 이를 배우고 사용하고 있습니다. 정규 표현식의 패턴을 이해하고 응용할 수 있는 능력은 PHP 프로그래밍에서 매우 유용합니다. 따라서 시간을 내어 몇 가지 패턴 예제를 직접 작성해보며 연습하는 것이 추천됩니다.

# 관련 자료

- [PHP 공식 문서 - 정규 표현식](https://www.php.net/manual/en/book.pcre.php)
- [정규 표현식을 활용한 패턴 매칭 툴](https://regex101.com/)
- [정규 표현식에 대한 더 많은 예제 및 설명](https://www.regular-expressions.info/tutorials.html)
- [정규 표현식을 자세히 다루는 책 "정규 표현식 완전 정복"](https://www.oreilly.com/library/view/regular-expressions-cookbook/9780596802837/)