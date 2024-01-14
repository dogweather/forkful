---
title:                "PHP: 텍스트 검색과 교체"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

PHP 프로그래밍에는 많은 작업이 필요합니다. 그 중에서도 가장 흔한 작업 중 하나는 텍스트를 찾고 바꾸는 것입니다. 이 작업은 반복적이고 지루할 수 있지만, 당신이 원하는 대로 텍스트를 쉽게 바꿀 수 있도록 도와줍니다. 따라서 이번 포스트에서는 왜 이 문제를 해결해야 하는지 알아보겠습니다.

## 어떻게

텍스트 검색 및 교체는 기능이 강력한 PHP 문자열 함수를 사용하여 쉽게 수행할 수 있습니다. 다음은 몇 가지 예시입니다.

```
<?php
// 원본 텍스트 설정
$text = "안녕하세요, 오늘 날씨는 매우 좋습니다.";

// 찾을 문자열 선택
$search = "매우 좋습니다.";

// 바꿀 문자열 선택
$replace = "정말 멋집니다.";

// 원본 텍스트에서 바꾸고자 하는 문자열을 찾아서 바꿔줍니다.
$new_text = str_replace($search, $replace, $text);

// 결과 출력
echo $new_text; // 안녕하세요, 오늘 날씨는 정말 멋집니다.
?>
```

위 예시에서는 `str_replace()` 함수를 사용하여 텍스트를 찾아서 바꿔줍니다. 이 함수는 원본 텍스트에서 원하는 문자열을 찾아서 새로운 문자열로 바꿔주는 기능을 합니다. 따라서 필요한 만큼 반복해서 사용할 수 있습니다. 또 다른 기능을 보여주는 예시를 살펴보겠습니다.

```
<?php
// 원본 텍스트 설정
$text = "내 이름은 John Smith입니다.";

// strpos 함수를 사용하여 "John"이라는 텍스트가 어디에 있는지 확인합니다.
$position = strpos($text, "John");

// 새로운 이름 설정
$new_name = "Jane";

// 원본 텍스트에서 John을 Jane으로 바꿔줍니다.
$new_text = substr_replace($text, $new_name, $position, 4);

// 결과 출력
echo $new_text; // 내 이름은 Jane Smith입니다.
?>
```

위 예시에서는 `strpos()`와 `substr_replace()` 함수를 사용하여 특정 위치에 있는 문자열을 바꾸는 방법을 보여줍니다. 이 외에도 `str_replace()` 함수를 사용하여 특정 문자열을 모두 바꿀 수도 있고, 정규식을 사용하여 패턴에 맞는 텍스트를 바꿀 수도 있습니다.

## 깊이 파고들기

PHP에서 문자열 검색 및 교체 기능을 수행하는 함수는 다양합니다. 이번 포스트에서는 몇 가지만 간단히 소개할 수 있었지만, 더 많은 옵션이 있으니 관련 문서를 참고하시는 것을 추천합니다. 또한, 이 기능을 활용하여 좀 더 복잡한 작업을 수행하는 방법도 있으니 더 많은 실습을 통해 습득하는 것이 좋습니다.

## 관련 링크

- [PHP 공식 문서 - 문자열 함수](https://www.php.net/manual/kr/ref.strings.php)
- [W3Schools - PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)
- [생활코딩 - PHP 문자열 함수 강좌](https://opentutorials.org/course/62)