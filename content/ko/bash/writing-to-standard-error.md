---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
표준 오류 출력(standard error)은 에러 메시지를 전달하기 위해 사용됩니다. 프로그래머는 문제를 신속하게 진단하고 로그 파일을 깨끗하게 유지하기 위해 표준 출력과 별도로 오류를 이곳에 씁니다.

## How to:
```Bash
#!/bin/bash

# 표준 출력 예제
echo "이것은 표준 출력입니다."

# 표준 오류 출력 예제
echo "이것은 표준 오류입니다." >&2

# 사용 예시
if [[ -z "$1" ]]; then
    echo "사용법: $0 파일명" >&2
    exit 1
fi
```

출력 예제:
```
이것은 표준 출력입니다.
이것은 표준 오류입니다.
```

## Deep Dive
표준 오류는 유닉스(Unix)와 리눅스(Linux) 시스템의 초기부터 사용되었습니다. `2>`를 이용하여 오류 출력을 파일이나 다른 명령어로 리다이렉트할 수 있습니다. `1>` 또는 `>`를 사용하여 표준 출력을 리다이렉트할 수 있으며, 파이프(`|`)는 기본적으로 표준 출력만을 다음 명령어로 넘깁니다. 표준 오류를 다루는 다른 방법으로는 `stderr 2>&1`과 같이 표준 오류를 표준 출력과 병합할 수도 있습니다.

## See Also
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Stack Overflow Questions on Bash: https://stackoverflow.com/questions/tagged/bash
