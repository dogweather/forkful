---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는다는 것은 문자열에 포함된 문자의 수를 구하는 것을 의미합니다. 프로그래머들이 이를 수행하는 이유는 입력 검증, 조건 분기 및 데이터 처리에서 주로 사용하기 때문입니다.

## 어떻게 하는가:

PHP에서 문자열의 길이를 알아보려면 `strlen()` 함수를 사용할 수 있습니다. 이 코드 예제를 확인 해 보시죠.

```PHP
$text = "안녕하세요!";
echo strlen($text);
```

이 스크립트를 실행하면 "21"이라는 결과가 출력됩니다. 왜냐하면 PHP는 기본적으로 바이트 수를 세기 때문입니다. 

한글 문자는 UTF-8에서 3바이트를 차지합니다. 그러므로 우리가 보기에 "안녕하세요!"는 6글자지만 실제로는 18바이트입니다. 끝에 느낌표가 있으니까 총 21바이트가 됩니다.

## 깊이 들어가기:

PHP의 `strlen()` 함수는 PHP 4버전부터 사용할 수 있었고, 그 때부터 문자열의 길이를 찾는 주요한 방법으로 사용되었습니다. 

자, `mb_strlen()` 함수에 대해 들어보신 적 있나요? 이 함수는 멀티바이트 문자열의 길이를 얻는 데 사용됩니다. 한글과 같이 멀티바이트 문자에 대해 정확한 문자 수를 세려면 이 함수를 사용하면 됩니다.

```PHP
$text = "안녕하세요!";
echo mb_strlen($text, "UTF-8");
```

이 스크립트를 실행하면 "6"이라는 결과를 얻습니다. 이는 우리가 기대하는 결과이며, 글자 수를 정확하게 보여줍니다.

## 참고자료:

문자열 조작에 대한 더 깊은 이해를 위해 다음 링크를 참조하십시오.

1. PHP Manual - String: https://www.php.net/manual/en/book.strings.php

2. PHP Manual - `strlen()`: https://www.php.net/manual/en/function.strlen.php

3. PHP Manual - `mb_strlen()`: https://www.php.net/manual/en/function.mb-strlen.php