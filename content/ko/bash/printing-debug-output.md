---
title:                "디버그 출력 출력하기"
html_title:           "Bash: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

만약 당신이 Bash 프로그래밍을 하고 있다면, 디버그 출력을 적절히 활용하는 것은 중요한 기술입니다. 디버그 출력은 코드가 작동하는 방식을 디버깅하는 데 도움을 줄 뿐만 아니라, 코드를 이해하고 새로운 기능을 추가하는 데에도 유용합니다.

## 사용법

디버그 출력을 사용하려면, "echo" 명령어를 사용하면 됩니다. 예를 들어, 만약 변수 "name"에 저장된 값이 무엇인지 출력하고 싶다면 아래와 같이 입력할 수 있습니다.

```Bash
echo "The name is $name"
```

위 코드는 "The name is" 다음에 변수 "name"의 값을 출력합니다. 따라서 "The name is John" 과 같은 결과를 얻게 됩니다.

## 심층 탐구

디버그 출력은 디버깅을 위해 도움이 되는 간단한 방법이지만, 더 많은 정보를 얻기 위해서는 "set" 명령어를 사용해야 합니다. "set -x"를 입력하면 스크립트 실행중에 각 줄이 실행되는 과정을 모두 출력해줍니다. "set -v"를 입력하면 실행되는 명령어를 모두 출력해줍니다. 이를 활용하면, 코드가 작동하는 방식을 자세히 파악할 수 있습니다.

## 참고

이 글에서 소개한 내용 외에도, Bash에서 디버깅을 위해 다양한 방법을 활용할 수 있습니다. 아래의 링크들에서 더 많은 정보를 찾아보세요.

- [Bash Reference Manual: Debugging](https://www.gnu.org/software/bash/manual/html_node/Debugging-Bash-Scripts.html)
- [Bash programming cheatsheet](https://devhints.io/bash)
- [Advanced Bash-Scripting Guide: Debugging](http://www.tldp.org/LDP/abs/html/debugging.html#DEBUGTRAP)