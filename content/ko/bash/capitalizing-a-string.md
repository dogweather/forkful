---
title:                "Bash: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

우리는 많은 프로그래밍 언어로써, 문자열에 특정한 동작을 적용할 수 있습니다. 그 중 하나는 대소문자 변환입니다. 대소문자 변환을 왜 해야하는지, 그리고 Bash로 어떻게 할 수 있는지, 그리고 심층적으로 살펴보겠습니다.

## 왜

대소문자 변환은 문자열을 깔끔하게 나타내기 위한 일반적인 방법 중 하나입니다. 예를 들어, 사용자의 입력이나 파일의 이름을 표준 표기법에 맞게 변환할 수 있습니다.

## 어떻게

Bash에서는 `tr` 명령어를 사용하여 문자열의 대소문자를 변환할 수 있습니다. 여러분은 문자열을 큰 따옴표로 감싸야 합니다. 아래의 예시를 참고하세요.

```Bash
echo "Hello WoRLd" | tr '[:lower:]' '[:upper:]'
```

출력:

```Bash
HELLO WORLD
```

또한 `sed` 명령어를 사용하여 문자열 내에서 특정 패턴의 대소문자를 변환할 수 있습니다. 아래의 예시를 참고하세요.

```Bash
echo "Hello WoRLd" | sed -e 's/h[aeiou]*/H/'
```

출력:

```Bash
HEllo WORLD
```

## 심층적으로 살펴보기

Bash에서 문자열을 다루는 다양한 방법이 있습니다. `tr`과 `sed` 외에도, `awk`와 `echo` 등 다른 명령어들을 사용하여 대소문자를 변환할 수 있습니다. 또한 Bash 변수를 사용하여 문자열 내에서 원하는 값을 대소문자로 변환할 수도 있습니다.

See Also:

- Bash 전체 문법 가이드: https://www.gnu.org/software/bash/manual/bash.html
- Bash 문자열 처리 관련 명령어 목록: https://www.shellhacks.com/bash-string-manipulation-examples/