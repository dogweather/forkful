---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

--- 

## 무엇 & 왜?

문자열을 소문자로 변환하는 것은 모든 문자를 소문자로 변경하는 프로세스입니다. 이는 데이터를 균일하게 만들어, 분석하거나 비교하는 데 도움이 되기 때문에 프로그래머들이 자주 사용합니다.

## 어떻게 하나:

Bash에서 문자열을 소문자로 변환하는 방법에는 여러 가지가 있습니다. 여기 몇 가지 예를 보여드리겠습니다.

```Bash
# tr command를 사용해 변환
string="HELLO WORLD"
echo "${string}" | tr '[:upper:]' '[:lower:]'

# 출력: hello world
```

```Bash
# Bash 내장 명령인,,를 사용해 변환
string="HELLO WORLD"
echo "${string,,}"

# 출력: hello world
```

## 깊게 알아보기:

문자열을 소문자로 변환하는 방법은 많은 프로그래밍 언어가 제공하며, 이는 아주 오래전부터 프로그래머들에게 중요한 도구였습니다. 'tr' 명령은 Unix's 표준 유틸리티 중 하나로, 아주 오래전부터 사용되었습니다. 반면에, Bash의 경우 '4.0' 버전부터 'string,,' 명령을 소개했습니다.

대안으로는 'awk' 또는 'sed' 등 다른 Unix 유틸리티를 사용할 수 있습니다. 'awk'와 'sed'는 각각 문자열 처리와 스트림 편집을 위해 설계되었습니다.

```Bash
# awk 사용
string="HELLO WORLD"
echo "${string}" | awk '{print tolower($0)}'

# 출력: hello world
```

```Bash
# sed 사용
string="HELLO WORLD"
echo "${string}" | sed 's/.*/\L&/'

# 출력: hello world
```

## 참고자료:

- Bash 소문자 변환에 대한 GNU 문서: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- 'tr' 명령에 대한 GNU 문서: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation
- 'awk' 사용법: https://www.gnu.org/software/gawk/manual/gawk.html
- 'sed' 사용법: https://www.gnu.org/software/sed/manual/sed.html