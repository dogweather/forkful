---
title:                "문자열의 길이 찾기"
html_title:           "Bash: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것이 왜 중요한지 궁금하지 않으신가요? 문자열의 길이를 알면 텍스트를 조작하거나 검색할 때 매우 유용합니다!

## 방법

문자열의 길이를 찾는 가장 간단한 방법은 `expr` 명령어를 사용하는 것입니다. 아래의 예시 코드를 참고해보세요.

```Bash
# 문자열을 변수로 선언합니다.
word="안녕하세요!"

# expr을 사용해 문자열의 길이를 찾습니다.
expr length "$word"
```

출력 결과는 다음과 같을 것입니다.

```Bash
7
```

만약 여러 줄로 이루어진 문자열의 길이를 찾고 싶다면 다음의 코드를 사용하시면 됩니다.

```Bash
# 여러 줄로 이루어진 문자열을 변수로 선언합니다.
multi_line="오늘은
날씨도
매우
좋아요!"

# wc 명령어를 사용해 문자열의 줄 수를 찾습니다.
echo "$multi_line" | wc -l
```

출력 결과는 다음과 같을 것입니다.

```Bash
4
```

## 더 들어가기

문자열을 구성하는 각각의 문자들은 아스키 코드로 표현됩니다. 이를 활용하면 문자열의 길이를 바이트 단위로 찾을 수도 있습니다. 또한, `awk`와 `sed`와 같은 다른 명령어도 문자열의 길이를 찾는 데에 유용하게 활용할 수 있습니다.

## 참고 자료

- [Bash 공식 홈페이지](https://www.gnu.org/software/bash/)
- [Bash 초보자를 위한 자습서](https://wiki.kldp.org/HOWTO/html/Adv-Bash-Scr-HOWTO/intro.html)