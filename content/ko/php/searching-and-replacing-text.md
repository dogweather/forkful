---
title:                "텍스트 검색 및 교체"
html_title:           "PHP: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

더 쉽게 말해, 텍스트를 찾아서 바꾸는 것은 웹 개발에서 매우 중요한 과정입니다. 이것은 사용자가 입력한 정보를 정리하고, 문자열을 수정하고, 코드를 업데이트하는 데 필수적입니다.

## 이렇게 하는 방법

제일 쉬운 방법은 `str_replace()` 함수를 사용하는 것입니다. 이 함수는 찾을 문자열, 바꿀 문자열, 대상 문자열이 필요합니다. 예시 코드는 다음과 같습니다.

```PHP
$search = '안녕';
$replace = 'hello';
$string = '안녕하세요';
$result = str_replace($search, $replace, $string);
echo $result; // output: hello하세요
```

위의 예시 코드에서 `str_replace()` 함수는 "안녕"을 "hello"로 바꾼 다음, 결과로 "hello하세요"를 출력합니다. 

또 다른 방법은 `preg_replace()` 함수를 사용하는 것입니다. 이 함수는 정규표현식을 사용하여 문자열을 바꿀 수 있습니다. 예시 코드는 다음과 같습니다.

```PHP
$search = '/[0-9]/';
$replace = '';
$string = 'abc123';
$result = preg_replace($search, $replace, $string);
echo $result; // output: abc
```

위의 예시 코드에서 `preg_replace()` 함수는 문자열에서 숫자를 찾아서 제거한 후, 결과로 "abc"를 출력합니다. 정규표현식에 대한 더 자세한 정보는 아래 "깊이 파헤치기" 섹션에서 다루겠습니다.

## 깊이 파헤치기

검색과 교체는 웹 개발에서 가장 기본적인 작업 중 하나입니다. 앞서 언급한 `str_replace()`와 `preg_replace()` 함수 외에도 많은 다른 함수들이 있습니다. 예를 들어, `str_ireplace()`는 대소문자를 구분하지 않고 문자열을 찾아 교체할 수 있습니다. 또한 `strtr()` 함수는 여러 개의 교체 규칙을 배열로 받아서 문자열을 교체할 수 있습니다. 

또한 정규표현식은 검색과 교체에서 더욱 강력한 도구입니다. PHP에서는 `preg_match()`를 사용하여 정규표현식으로 검색한 결과를 배열로 받아올 수 있습니다. `preg_replace()`를 사용하여 정규표현식으로 문자열을 교체할 수도 있습니다. 하지만 정규표현식을 사용할 때는 패턴을 제대로 이해하고 작성해야 합니다. 더 자세한 정규표현식 사용법은 아래 "참고자료" 섹션에서 제공하는 링크들을 참고해주세요.

## 참고자료

- PHP 공식 문서 - [Strings](https://www.php.net/manual/en/language.types.string.php)
- PHP 공식 문서 - [Regular Expressions](https://www.php.net/manual/en/ref.pcre.php)
- TutorialsPoint - [PHP 문자열 함수](https://www.tutorialspoint.com/php/php_string_functions.htm)
- W3Schools - [PHP 정규표현식](https://www.w3schools.com/php/php_regex_intro.asp)