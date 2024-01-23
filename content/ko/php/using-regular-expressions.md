---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾고 조작할 수 있는 강력한 도구입니다. 프로그래머들은 복잡한 텍스트 처리를 빠르고 효율적으로 수행할 수 있도록 하기 위해 사용합니다.

## How to: (방법)
```php
<?php
$str = "안녕하세요, 저는 PHP 개발자입니다.";
$pattern = "/개발자/";

// 패턴 매칭
if (preg_match($pattern, $str)) {
    echo "그 문장에는 '개발자'라는 단어가 있습니다.";
} else {
    echo "매칭되는 단어가 없습니다.";
}

// 패턴 치환
$replacedStr = preg_replace($pattern, '프로그래머', $str);
echo $replacedStr; // 출력: 안녕하세요, 저는 PHP 프로그래머입니다.
?>
```

## Deep Dive (심층 탐구)
정규 표현식은 1950년대 초반부터 사용되기 시작했고, 이후 많은 프로그래밍 언어에서 지원하고 있습니다. PHP에서는 `preg_*` 함수 계열을 통해 PCRE(Perl Compatible Regular Expressions)를 구현합니다. `preg_match`, `preg_replace`, `preg_split`과 같은 함수들은 텍스트를 정렬하고 필터링할 때 매우 유용합니다. 문자열 함수나 `explode`, `str_replace` 같은 대안들도 있지만 정규 표현식은 복잡한 문자열 작업에 더 적합합니다.

## See Also (더 보기)
- PHP 정규 표현식 공식 문서: [PHP Manual on PCRE](https://www.php.net/manual/en/book.pcre.php)
- 정규 표현식 학습: [RegexOne](https://regexone.com)
- 정규 표현식 테스트 도구: [RegExr](https://regexr.com)
