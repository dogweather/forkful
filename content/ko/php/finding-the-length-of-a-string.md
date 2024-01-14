---
title:                "PHP: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

왜: 문자열의 길이를 찾는 것에 대해 이유를 설명합니다.

문자열의 길이를 찾는 것은 다양한 이유로 필요할 수 있습니다. 예를 들어, 입력받은 사용자 이름이나 이메일 주소의 길이를 확인하여 유효성 여부를 검사할 수 있습니다. 또는 특정한 문자열의 길이를 제한하고 싶을 때 유용합니다.

## 어떻게

생각보다 문자열의 길이를 찾는 것은 간단한 작업입니다. PHP의 내장 함수인 `strlen()`을 사용하면 됩니다. 아래는 예제 코드와 실행 결과입니다.

```PHP
<?php

// 문자열의 길이를 출력하는 예제
$name = "홍길동";
echo "이름의 길이는 " . strlen($name) . "입니다.";
```

실행 결과:
```
이름의 길이는 3입니다.
```

## 딥 다이브

`strlen()` 함수는 문자열의 실제 길이를 반환합니다. 이 함수는 문자열에서 유효한 글자의 개수를 세는 것으로, 공백이나 특수 문자는 제외됩니다. 만약 한글을 사용하는 경우, 한 글자가 2바이트로 취급되기 때문에 한 글자당 2바이트를 반환합니다.

PHP의 다른 내장 함수인 `mb_strlen()`은 다중 바이트 문자를 올바르게 계산하여 문자열의 길이를 반환합니다. 위 예제에서 `strlen()`을 `mb_strlen()`으로 바꾸면 올바른 결과를 얻을 수 있습니다.

## See Also

- PHP `strlen()` 함수 문서: https://www.php.net/manual/en/function.strlen.php
- PHP `mb_strlen()` 함수 문서: https://www.php.net/manual/en/function.mb-strlen.php