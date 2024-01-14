---
title:                "PHP: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 substring을 추출할까요?

문자열을 다루는 프로그래밍에서, substring(부분 문자열)을 추출하는 것은 매우 유용한 작업입니다. 예를 들어, 사용자가 입력한 전화번호에서 지역 번호를 추출하거나, 이메일 주소에서 도메인을 추출하는 등 다양한 상황에서 substring을 추출하는 작업이 필요할 수 있습니다.

# substring 추출하는 방법

substring을 추출하는 방법은 간단합니다. PHP에서는 ```substr()``` 함수를 사용하면 됩니다. 이 함수는 3개의 인자를 받는데, 첫 번째로는 추출하고 싶은 문자열을, 두 번째로는 시작 위치를, 세 번째로는 추출할 길이를 매개변수로 받습니다.

```
<?php
$string = "안녕하세요, PHP!";
$substring = substr($string, 0, 3);
echo $substring; // 출력 결과: 안녕
?>
```

위 코드에서는 ```substr()``` 함수를 사용하여 처음부터 3개의 문자를 추출하는 예시입니다. 시작 위치를 0으로 지정하면 문자열의 첫 번째 문자부터 추출할 수 있습니다. 추출할 길이를 지정하지 않으면 시작 위치부터 끝까지의 문자열이 추출됩니다.

# substring 추출의 깊은 이해

substring을 추출하는 작업은 보다 복잡한 문자열 조작에 사용될 수 있습니다. 예를 들어, 문자열 안에 특정 단어가 들어있는지 확인하기 위해 ```substr()``` 함수를 사용해 길이를 지정하면, 해당 단어가 문자열에 포함되어있지 않으면 문자열의 길이가 반환되는 원리를 이해하는 것이 중요합니다.

또한, PHP에서는 추출할 시작 위치를 음수로 지정할 수도 있습니다. 이 경우에는 문자열의 뒤에서부터 추출하게 됩니다. 예를 들어, ```substr($string, -3)```은 문자열의 마지막 3글자를 추출하는 것과 동일합니다.

# 더 많은 정보 알아보기

substring을 추출하는 것 외에도, PHP에서는 문자열을 조작하는 다양한 함수들이 있습니다. ```strpos()```를 사용하면 특정 문자열이 문자열 안에 어디에 위치하는지 알 수 있고, ```str_replace()```를 사용하면 문자열 안에서 특정 문자열을 다른 문자열로 바꿀 수 있습니다.

# 함께 보기 

- [PHP 문자열 처리 관련 내장 함수 목록](https://www.php.net/manual/kr/ref.strings.php)
- [PHP substring 관련 공식 문서](https://www.php.net/manual/kr/function.substr.php)