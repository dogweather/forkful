---
title:                "문자열 대문자로 변환하기"
html_title:           "PHP: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열을 대문자로 변환하는 것을 컴퓨터 프로그래밍에서 대문자 변환(capitalizing)이라고 합니다. 프로그래머들이 대문자 변환을 하는 이유는 검색 엔진과 같은 기능을 할 때 문자열의 대소문자를 구별해야 하기 때문입니다. 예를 들어, 'apple'과 'Apple'은 다른 단어로 인식되지만, 대문자로 변환하면 둘 다 'APPLE'로 인식되어 하나의 단어로 취급할 수 있습니다.

## 하는 법:

PHP에서 문자열을 대문자로 변환하는 방법은 `strtoupper()` 함수를 사용하는 것입니다. 예를 들어, `strtoupper("apple")`을 실행하면 `APPLE`이라는 결과가 출력됩니다.

```PHP
<?php
echo strtoupper("apple"); // 결과: APPLE
?>
```

또는, 문자열 변수에 할당하여 사용할 수도 있습니다.

```PHP
<?php
$string = "apple";
echo strtoupper($string); // 결과: APPLE
?>
```

## 깊이 들어가보기:

대문자 변환은 운영체제에 따라 다른 결과를 출력할 수도 있습니다. 예를 들어, 윈도우 운영체제에서는 한글의 경우 `strtoupper()` 함수를 통해 대문자로 변환하지 않고 그대로 출력됩니다. 그러나 리눅스 운영체제에서는 대문자로 변환됩니다. 이는 운영체제가 문자 인코딩 방식이 다르기 때문입니다. 또한, `strtoupper()` 함수는 영문자 외의 문자에 대해 제대로 작동하지 않을 수 있으므로 이 점을 명심해야 합니다.

대문자 변환 외에도 소문자 변환 함수인 `strtolower()`도 있습니다. 두 함수는 문자열 안의 모든 문자를 대문자 또는 소문자로 변환해주는 기능을 합니다.

## 관련 링크:

- PHP `strtoupper()` 함수 설명서: https://www.php.net/manual/en/function.strtoupper.php
- PHP `strtolower()` 함수 설명서: https://www.php.net/manual/en/function.strtolower.php