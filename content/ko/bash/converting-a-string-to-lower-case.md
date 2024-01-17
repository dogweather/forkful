---
title:                "문자열 소문자로 변환하기"
html_title:           "Bash: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

문자열을 소문자로 변환하는 것은 프로그래머들이 자주 하는 작업입니다. 이는 대소문자를 구분하지 않는 프로그래밍 언어에서 일관성을 유지하거나 문제를 해결하기 위해 사용될 수 있습니다. 예를 들어, 사용자의 입력을 받을 때 대소문자를 구분하지 않기 위해 모든 문자열을 소문자로 변환하여 검사할 수 있습니다. 또는 검색 엔진에서 특정 키워드를 찾을 때 대소문자를 구분하지 않도록 검색어를 소문자로 변환할 수 있습니다.

## 방법:

Bash에서는 문자열을 소문자로 변환하는 여러 가지 방법이 있습니다. 가장 간단한 방법은 `tr` 명령어를 사용하는 것입니다. `tr`은 입력 스트림에서 지정한 문자를 다른 문자로 변환하는 명령어입니다. 다음은 `tr`을 사용하여 문자열을 소문자로 변환하는 예시입니다.

```Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'

# 출력: hello world
```

또 다른 방법은 `tr` 대신 `sed` 명령어를 사용하는 것입니다. `sed`은 스트림 편집기로, 특정 정규 표현식을 사용하여 스트림에서 원하는 텍스트를 찾고 대체할 수 있습니다. 다음은 `sed`을 사용하여 문자열을 소문자로 변환하는 예시입니다.

```Bash
echo "HELLO WORLD" | sed 's/.*/\L&/'

# 출력: hello world
```

## 깊게 파고들기:

이러한 방법 외에도 문자열을 소문자로 변환하는 다른 방법들이 존재합니다. 예를 들어, `awk`과 `perl`은 각각 문자열을 소문자로 바꾸는 내장 함수를 제공합니다. 다만 `awk`은 대소문자를 구분하지 않는 고려하지 않으므로 실제로 소문자로 변환하는 것은 아닙니다.

또한, 운영 체제마다 사용 가능한 명령어가 다르기 때문에 `tr`과 `sed`을 사용하는 것이 가장 일반적인 방법입니다. 그러나 일부 다양한 언어와 프로그래밍 환경에서도 이와 유사한 함수를 제공할 수 있습니다.

마지막으로, 문자열을 소문자로 변환하는 것은 그 자체로는 큰 역할을 하지 않지만 프로그래밍에서 일관성을 유지하는 것에 도움을 줄 수 있습니다. 이는 코드 유지 및 디버깅 과정에서 도움이 될 수 있습니다.

## 관련 자료:

- [Bash tr command](https://www.tutorialspoint.com/unix_commands/tr.htm)
- [Bash sed command](https://www.tutorialspoint.com/unix_commands/sed.htm)
- [Bash awk command](https://www.tutorialspoint.com/unix_commands/awk.htm)
- [Perl lc function](https://perldoc.perl.org/perlop.html#lc)
- [Awk tolower function](https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html)