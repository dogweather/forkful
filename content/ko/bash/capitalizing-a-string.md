---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가와 왜 사용하는가?)
문자열을 대문자로 만드는 것은 모든 문자를 대문자 버전으로 변환하는 작업입니다. 가독성을 높이고, 사용자 인터페이스를 일관되게 유지하며, 프로그래밍 상에서는 특정 문자열을 상수로 표현할 때 자주 사용됩니다.

## How to: (방법)
Bash에서 문자열을 대문자로 변환하는 기본적인 명령어는 `tr`, `awk`, `sed`, 그리고 내장된 Bash 기능 등을 사용할 수 있습니다. 각각의 방법으로 "hello, world"를 대문자로 변환해보겠습니다.

```Bash
# tr을 사용하여 대문자로 변환
echo "hello, world" | tr '[:lower:]' '[:upper:]'

# awk을 사용하여 대문자로 변환
echo "hello, world" | awk '{print toupper($0)}'

# sed을 사용하여 대문자로 변환
echo "hello, world" | sed 's/.*/\U&/'

# Bash 내장 기능을 사용하여 대문자로 변환
str="hello, world"
echo "${str^^}"
```

출력은 모든 방법에서 같습니다:
```
HELLO, WORLD
```

## Deep Dive (심층 탐구)
초창기 유닉스 시스템에서는 문자열 처리가 매우 기본적이었습니다. 그러나 시간이 지나면서 여러 도구들이 개발되었는데, `tr`, `awk`, `sed`는 그 중 대표적인 문자열 처리 도구들입니다. 각 도구는 특정 작업에 최적화되어 있는데, 예를 들어 `tr`은 간단한 문자 변환에, `awk`는 더 복잡한 텍스트 추출과 처리에, 그리고 `sed`는 텍스트 스트림 편집에 적합합니다.

Bash 버전 4 이상부터는 문자열 조작이 더 간단해졌고, 파라미터 확장을 통해 대소문자 변경이 가능해졌습니다(`^^`은 대문자로, `,,'는 소문자로 변환).

그런데, 왜 이렇게 많은 방법이 있을까요? 다양한 옵션이 있다는 것은 다양한 문제 상황에 유연하게 대처할 수 있다는 의미입니다. `tr`이나 `awk`등이 설치되어 있지 않은 시스템에서도 Bash 내장 기능을 이용할 수 있으며, 일부 다른 유닉스 계열 시스템에서도 호환성을 제공합니다.

## See Also (더 보기)
- Bash 문자열 조작 가이드: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- AWK 사용법 가이드: https://www.gnu.org/software/gawk/manual/gawk.html
- SED 사용자 매뉴얼: https://www.gnu.org/software/sed/manual/sed.html