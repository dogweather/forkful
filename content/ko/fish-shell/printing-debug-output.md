---
title:                "디버그 출력하기"
html_title:           "Fish Shell: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래머들이 디버그 출력을 하는 이유는 무엇일까요? 간단하게 말해, 디버그 출력이란 쉽게 말해 코드의 문제를 찾기 위해 프로그램 실행 중에 뭔가를 출력하는 것을 말합니다. 따라서 디버그 출력은 코드를 디버깅하는 데 매우 유용한 도구입니다.

## 어떻게:
Fish Shell에서 디버그 출력을 하는 방법은 매우 간단합니다. 먼저, `echo` 명령어를 사용하여 원하는 내용을 출력합니다. 그리고 이 출력을 `SHELL_DEBUG` 환경 변수를 사용하여 간단하게 제어할 수 있습니다. 아래는 간단한 예시입니다.

```Fish Shell
set -x SHELL_DEBUG # SHELL_DEBUG 환경 변수 설정
echo "Debug 출력 예시"
set -e SHELL_DEBUG # SHELL_DEBUG 환경 변수 해제
```

위의 예시 코드를 실행하면, "Debug 출력 예시"가