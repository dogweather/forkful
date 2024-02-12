---
title:                "정규 표현식 사용하기"
aliases:
- /ko/php/using-regular-expressions/
date:                  2024-02-03T19:17:59.968008-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

PHP에서 정규 표현식(regex)은 문자열 내에서 문자 조합을 일치시키는 데 사용되는 패턴으로, 정교한 검색 및 교체 작업과 데이터 검증을 가능하게 합니다. 프로그래머는 텍스트 파싱, 양식 검증, 웹 데이터 스크래핑 등에 있어 그 강력함과 유연성 때문에 regex를 활용하며, 이는 개발자의 필수 도구가 되었습니다.

## 사용 방법:

PHP는 PCRE(Perl 호환 정규 표현식) 라이브러리를 통해 정규 표현식을 지원하며, 풍부한 함수 세트를 제공합니다. 사용 방법은 다음과 같습니다:

### 패턴 일치:

문자열 내에 패턴이 존재하는지 확인하려면 `preg_match()`를 사용합니다. 이 함수는 문자열에 패턴이 발견되면 1을, 발견되지 않으면 0을 반환합니다.

```php
if (preg_match("/\bweb\b/i", "PHP는 웹 스크립팅 언어입니다")) {
    echo "일치하는 항목이 발견되었습니다.";
} else {
    echo "일치하는 항목이 발견되지 않았습니다.";
}
// 출력: 일치하는 항목이 발견되었습니다.
```

### 모든 일치 항목 찾기:

문자열 내에 패턴의 모든 발생을 찾아야 할 때 `preg_match_all()`이 사용됩니다.

```php
$text = "고양이와 개";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// 출력: Array ( [0] => 고양이 [1] => 와 [2] => 개 )
```

### 텍스트 교체:

정규 표현식과 일치하는 텍스트를 교체하기 위해 `preg_replace()`가 사용됩니다. 데이터 포맷팅 및 정리에 매우 강력합니다.

```php
$originalText = "2003년 4월 15일";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// 출력: 2003년 4월1,15
```

### 문자열 분리:

`preg_split()`을 사용하여 문자열을 배열로 분리할 수 있으며, 구분자에 대한 패턴을 지정합니다.

```php
$text = "PHP는, 매우 인기 있는, 스크립팅 언어입니다";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// 출력: Array ( [0] => PHP는 [1] => 매우 인기 있는 [2] => 스크립팅 언어입니다 )
```

또한, 복잡한 정규 표현식 패턴과 작업을 위해 Symfony의 `Finder` 컴포넌트나 Laravel의 도우미 함수 모음과 같은 프레임워크와 라이브러리가 보다 편리한 추상화 계층을 제공할 수 있습니다. 그러나 PHP 스크립트 내에서 효율적인 텍스트 처리와 유효성 검사를 직접 수행하기 위해서는 PHP의 내장 PCRE 함수를 이해하고 활용하는 것이 중요합니다.
