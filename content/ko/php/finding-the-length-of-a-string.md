---
title:                "PHP: 문자열의 길이 찾기"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜
가끔은 문자열의 길이를 알아야 할 때가 있습니다. 예를 들어, 사용자가 입력한 비밀번호가 최소한 8자리 이상이어야 한다면, 그것을 검증하기 위해 문자열의 길이를 알 수 있어야 할 것입니다.

## 하는 방법
```PHP
<?php
$str = "안녕하세요";
echo strlen($str); // 이 코드는 문자열 "안녕하세요"의 길이인 5를 출력합니다.
?>
```

위의 예시 코드에서는 PHP 내장 함수인 `strlen()`을 사용하여 문자열의 길이를 구하는 방법을 보여줍니다. 이 함수는 인자로 받은 문자열의 길이를 반환해 줍니다. 참고로, 한글의 경우에는 영문과 달리 각 글자가 2바이트로 이루어져 있기 때문에, 실제로 10바이트의 길이를 가지지만, `strlen()` 함수는 글자 수를 기준으로 길이를 반환해 줍니다.

## 깊이 파헤치기
`strlen()` 함수는 매우 간단하고 유용한 함수이지만, 때로는 약간의 오류를 발생시킬 수 있습니다. 예를 들어, 문자열의 길이를 구하는 것이 아닌, 바이너리 데이터의 길이를 구하려 할 때 `strlen()`은 원하는 결과를 제공하지 못할 수 있습니다. 이럴 때에는 `mb_strlen()` 함수를 사용하는 것이 좋습니다. 이 함수는 문자열의 실제 길이를 확인하여 올바른 값을 반환해 줍니다.

# 같이 보기
- [PHP 문자열 함수](https://www.php.net/manual/en/ref.strings.php)
- [PHP mb_strlen() 함수 문서](https://www.php.net/manual/en/function.mb-strlen.php)
- [한글 문자열에서의 문자열 길이 구하기 (in Stack Overflow)](https://stackoverflow.com/questions/16805543/how-to-get-string-length-of-hangul-in-php)