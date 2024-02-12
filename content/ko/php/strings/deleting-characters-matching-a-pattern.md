---
title:                "패턴에 일치하는 문자 삭제"
aliases:
- /ko/php/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:39.454848-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열에서 패턴에 맞는 문자를 삭제하는 건 특정 형식의 데이터를 정리하거나 필요 없는 부분을 제거하기 위해 사용합니다. 프로그래머들은 이를 통해 입력 데이터를 깔끔하게 처리하고, 보안을 강화하기도 합니다.

## How to: (방법)

PHP에서는 `preg_replace` 함수를 이용해서 문자열에서 원하는 패턴을 찾아 삭제합니다. 아래 예제를 보세요.

```PHP
<?php
$text = "안녕하세요! 1234 년에 태어난 5678이에요.";
$pattern = '/[0-9]+/';

// 숫자를 모두 삭제합니다
$clean_text = preg_replace($pattern, '', $text);

echo $clean_text; // "안녕하세요!  년에 태어난 이에요."
?>
```

여기서 `/[0-9]+/`는 하나 이상의 숫자를 매칭하는 정규 표현식입니다. `preg_replace`는 이 패턴과 매칭되는 모든 부분을 빈 문자열('')로 대체해서 삭제합니다.

## Deep Dive (심층 붐뻡)

`preg_replace`는 PCRE(Perl Compatible Regular Expressions) 라이브러리를 기반으로 합니다. 이는 1980년대 Perl 언어에서 시작된 기능으로 오늘날 대부분의 프로그래밍 언어에 구현되어 있습니다.

대안으로 `str_replace`나 `str_ireplace` 함수를 사용할 수도 있습니다. 이 함수들은 간단한 문자열 치환에 사용하며, 정규 표현식은 사용하지 않습니다.

실행 속도를 생각하면, 복잡하지 않은 단순치환은 `str_replace`가 `preg_replace`보다 빠를 수 있습니다. 하지만 패턴 매칭이 필요하다면 `preg_replace`가 더 적합합니다.

## See Also (관련 자료)

- [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)
- [정규 표현식 - Tutorial](https://www.regular-expressions.info/tutorial.html)

이들 링크는 더 많은 정보와 다양한 예제를 제공하기 때문에 매우 유용합니다.
