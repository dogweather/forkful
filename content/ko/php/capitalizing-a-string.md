---
title:    "PHP: 문자열 대문자로 변환하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 변경하는 것에 대해 알아보겠습니다. 이 기능은 문자열을 이쁘게 포맷하거나 특정한 목적에 맞추기 위해 사용될 수 있습니다.

## 어떻게

PHP에서 문자열의 첫 글자를 대문자로 변경하기 위해서는 다음과 같은 코드를 사용할 수 있습니다.

```PHP
$string = "hello world";
echo ucfirst($string); // Output: Hello world
```

우리는 우선 대문자로 변경할 문자열을 변수에 저장한 뒤 `ucfirst()` 함수를 이용해 첫 글자를 대문자로 변경합니다.

## 깊이 파헤치기

PHP에서 많은 문자열 함수들 중 `ucfirst()` 함수는 대문자로 첫 글자를 변경해주는 기능을 가지고 있습니다. 하지만 이 함수는 첫 글자가 영문자일 경우에만 동작하기 때문에 유니코드 문자열이나 이모지를 처리하는 데에는 적합하지 않습니다. 이를 해결하기 위해서는 `mb_convert_case()` 함수를 사용할 수 있습니다.

예를 들어, 다음과 같은 문자열을 대문자로 변경해보겠습니다.

```PHP
$string = "안녕하세요";
echo mb_convert_case($string, MB_CASE_TITLE, "UTF-8"); // Output: 안녕하세요
```

`mb_convert_case()` 함수의 첫 번째 파라미터에는 대문자로 변경할 문자열이 입력되고, 두 번째 파라미터에는 `MB_CASE_TITLE` 상수를 지정해주어야 합니다. 마지막으로 문자열의 인코딩을 지정해주어야 하는데, 위 예제에서는 한글을 사용하기 때문에 `UTF-8`을 지정해주었습니다.

## 관련 자료

- [PHP 문자열 함수 문서](https://www.php.net/manual/en/ref.strings.php)
- [PHP mb_convert_case 함수 문서](https://www.php.net/manual/en/function.mb-convert-case.php)