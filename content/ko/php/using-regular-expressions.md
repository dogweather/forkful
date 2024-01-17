---
title:                "정규식 사용하기"
html_title:           "PHP: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식을 사용하면 특정 문자열 패턴을 검색하고 일치하는 부분을 찾을 수 있습니다. 프로그래머는 자주 반복되는 작업을 간단하게 처리할 수 있기 때문에 정규 표현식을 사용합니다.

## 하는 방법:
PHP의 `preg_match()` 함수를 사용하여 문자열에 패턴을 매칭할 수 있습니다.

```
// 문자열에서 숫자만 추출하는 예제
$string = "I have 3 apples and 5 oranges.";
preg_match("/\d+/", $string, $matches);
echo $matches[0]; // output: 3
```

## 깊이 들어가기:
정규 표현식은 부분적으로 1950년대에 완전히 발명되었습니다. 프로그래밍에서 정규 표현식은 일반적으로 문자열 검색 또는 교체를 위해 사용되지만 서로 다른 언어에서도 사용 가능한 다른 방법으로 이 작업을 수행할 수 있습니다. PHP에서는 정규 표현식을 `preg_*` 함수로 묶어 구현하기 때문에 간단하게 사용할 수 있습니다.

## 관련 자료:
- [PHP 정규 표현식 문서](https://www.php.net/manual/en/book.pcre.php)
- [정규 표현식 테스트 사이트](https://regex101.com/)