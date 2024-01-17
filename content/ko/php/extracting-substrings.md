---
title:                "부분 문자열 추출하기"
html_title:           "PHP: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 뭐고 왜 하는가?
문자열에서 일부분을 추출하는 것이 무엇인지 그리고 프로그래머들이 그것을 왜 하는지 설명해보겠습니다.

## 어떻게 해야 하나요?
`PHP` 코드 블록 내부에서 코딩 예제와 출력 결과를 함께 볼 수 있습니다.

```PHP
// 문자열에서 일부분을 추출하는 방법
$str = "Hello World";

echo substr($str, 0, 5); // 출력 결과: "Hello"
echo substr($str, 6); // 출력 결과: "World"
```

## 깊이 파고들기
문자열에서 일부분을 추출하는 것은 언제부터 사용되었고, 대안은 무엇인지 그리고 구현에 대한 자세한 정보를 알아보겠습니다.

1. 역사적 맥락: 문자열에서 일부분을 추출하는 기능은 이미 오래 전부터 사용되었습니다. 이 기능은 문자열을 다루는 작업에서 필수적이기 때문에 거의 모든 언어에서 지원합니다.
2. 대안: `substr()` 함수는 문자열에서 일부분을 추출하는 가장 많이 사용되는 메소드 중 하나입니다만, `preg_match()` 함수를 사용해서 정규식을 이용하여 문자열을 추출하는 방법이 있습니다.
3. 구현 방법: `substr()` 함수는 `$start`와 `$length` 매개변수를 사용합니다. `$start`는 추출하고자 하는 문자열의 시작 위치를 나타내고, `$length`는 추출하고자 하는 길이를 나타냅니다. 또는 `$length` 대신 음수 값을 넣어서 문자열의 뒤에서부터 추출할 수도 있습니다.

## 관련 자료
연관된 정보를 알아보기 위해 아래 링크를 확인해보세요.

- PHP `substr()` 함수 문서: https://www.php.net/manual/en/function.substr.php
- PHP `preg_match()` 함수 문서: https://www.php.net/manual/en/function.preg-match.php