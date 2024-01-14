---
title:                "PHP: 패턴과 일치하는 글자 삭제하기"
simple_title:         "패턴과 일치하는 글자 삭제하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

패턴에 일치하는 문자를 삭제하는 것에 관심을 가지는 이유는 주로 데이터 정제나 가공에 있습니다. 예를 들어, 어떤 텍스트 파일에서 특정한 단어를 모두 삭제하고 싶을 때 등의 경우에 이 기술을 사용할 수 있습니다.

# 방법

```PHP
// 입력 문자열
$input = "Hello World! This is a test string.";

// 정제할 패턴
$pattern = '/[a-zA-Z]/';

// 입력 문자열에서 패턴에 일치하는 문자를 모두 삭제
$output = preg_replace($pattern, "", $input);
echo $output; // 결과: "!  ."

```

위의 예제 코드에서는 `preg_replace()` 함수를 사용하여 입력 문자열에서 정규표현식 패턴에 일치하는 문자를 모두 삭제하는 방법을 보여줍니다. 이를 응용하여 다양한 패턴을 사용하면 원하는 결과를 얻을 수 있습니다. 또한 `preg_replace()` 함수를 사용할 때는 정규표현식의 문법을 알아야 하며, 이를 사용하여 다양한 패턴을 만들 수 있습니다.

# 심층 분석

패턴에 일치하는 문자를 삭제하는 방법은 정규표현식(Regular Expression)이라는 개념과 밀접한 관련이 있습니다. 정규표현식은 문자열에서 특정한 패턴을 찾거나 대체하는 작업을 수행할 때 유용하게 사용할 수 있는 도구입니다. 정규표현식의 문법은 다소 복잡할 수 있지만, 한 번 익혀두면 다양한 상황에서 유용하게 사용할 수 있습니다.

# 관련 링크

- [PHP 정규표현식 가이드 (한국어)](https://www.php.net/manual/kr/book.pcre.php)
- [정규표현식 30분 학습 (영어)](https://regexone.com/)
- [정규표현식 실습 사이트 (영어)](https://regexr.com/)