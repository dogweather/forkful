---
title:                "부분 문자 추출"
html_title:           "PHP: 부분 문자 추출"
simple_title:         "부분 문자 추출"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열 추출을 수행하는 이유는 다양합니다. 예를 들어, 특정 문자열을 검색하거나, 문자열을 비교하거나, 문자열의 일부분을 수정해야 할 때 사용할 수 있습니다.

## 사용 방법

PHP에서 문자열 추출을 수행하는 방법은 간단합니다. 우선, 추출하고 싶은 문자열을 변수에 할당합니다. 그리고 `substr()` 함수를 사용하여 해당 문자열의 일부분을 추출할 수 있습니다.

예를 들어, 다음과 같이 할 수 있습니다.

```PHP
$str = "안녕하세요! 반가워요.";
echo substr($str, 0, 5); // "안녕하세요"
echo substr($str, 6); // "반가워요."
```

첫 번째 매개변수로는 추출하고 싶은 문자열을, 두 번째 매개변수로는 추출하고 싶은 시작 위치를, 세 번째 매개변수로는 추출하고 싶은 최대 길이를 전달합니다. 세 번째 매개변수는 옵셔널하며, 전달하지 않을 경우 끝까지 추출됩니다.

가끔은 특정 문자열이 몇 번째 위치에 있는지도 알아야 할 때가 있습니다. 이 경우에는 `strpos()` 함수를 사용하여 해당 문자열의 위치를 알 수 있습니다. 예를 들어, 다음과 같이 할 수 있습니다.

```PHP
$str = "오늘은 날씨가 좋네요";
echo strpos($str, "날씨"); // 5
```

또한, 특정 문자열이 포함되는지 여부를 확인할 때에는 `strstr()` 함수를 사용할 수 있습니다. 예를 들어, 다음과 같이 할 수 있습니다.

```PHP
$str = "I love coding";
echo strstr($str, "love"); // "love coding"
```

## 깊이 파헤치기

문자열 추출은 PHP에서 매우 유용한 기능 중 하나입니다. 그만큼 PHP에서 문자열 다루기가 중요하다는 것을 알 수 있습니다.

추출할 때는 다양한 옵션을 사용할 수 있습니다. 예를 들어, 음수를 전달할 경우 끝에서부터 추출할 수 있습니다.

```PHP
$str = "Hello World";
echo substr($str, -5); // "World"
```

또한, 추출한 문자열을 가공하는 것도 가능합니다. 예를 들어, `ucfirst()` 함수를 사용하여 추출한 문자열의 첫 번째 글자를 대문자로 바꿀 수 있습니다.

```PHP
$str = "hello world";
echo ucfirst(substr($str, 0, 5)); // "Hello"
```

마지막으로, 추출한 문자열을 변수에 다시 할당하여 활용할 수도 있습니다. 예를 들어, 다음과 같이 할 수 있습니다.

```PHP
$str = "Hello World";
$first = substr($str, 0, 5); // "Hello"
$last = substr($str, -5); // "World"
$newStr = $last . " " . $first; // "World Hello"
echo $newStr;
```

## 관련 자료

- [PHP 공식 문서 - substr() 함수](https://www.php.net/manual/en/function.substr.php)
- [PHP 공식 문서 - strpos() 함수](https://www.php.net/manual/en/function.strpos.php)
- [PHP 공식 문서 - strstr() 함수](https://www.php.net/manual/en/function.strstr.php)