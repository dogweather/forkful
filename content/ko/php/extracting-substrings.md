---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

서브스트링 추출은 주어진 문자열에서 특정 부분 문자열을 가져오는 것을 말합니다. 프로그래머들이 이를 사용하는 주요한 이유는 문자열 분석 및 처리 작업에서 특정 데이터를 선택할 필요성 때문입니다.

## 방법:

```PHP
<?php
  $str = "Hello, World!";
  echo substr($str, 7, 5);
?>
```

위 코드의 결과는 "World"입니다. `substr` 함수는 첫 번째 매개변수로 전체 문자열을, 두 번째 매개변수는 시작 위치를, 세 번째 매개변수는 가져올 문자열의 길이를 나타냅니다.

```PHP
<?php
  $str = "Hello, World!";
  echo substr($str, -1);
?>
```

위 코드의 결과는 "!"입니다. 음수 인덱스는 문자열 끝에서 부터의 위치를 나타냅니다.


## 깊이 들여다보기:

서브스트링 추출은 문자열 처리에 있어 중대한 역할을 합니다. PHP에선 `substr` 함수를 사용해 이 작업을 달성했습니다. 2004년 PHP 5에서 처음 도입됐으며, 그 후로 개발자들에게 많은 편리함을 제공했습니다.

대안으로 `mb_substr`, `substr_replace`, `strpos` 등의 함수가 있습니다. 이들은 다양한 문자열 조작과 관련된상황에서 사용됩니다.

실제로, 강력한 라이브러리와 함수를 사용하여 PHP의 부분 문자열 추출은 그 구현이 단순하고 실행시간이 매우 빠릅니다.

## 참고 자료:

- PHP 공식 문서의 `substr` 함수 [링크](https://www.php.net/manual/en/function.substr.php)
- `mb_substr`, `substr_replace`, `strpos` 함수에 대한 자세한 정보는 PHP 공식 문서 [링크](https://www.php.net/manual/en/book.mbstring.php)