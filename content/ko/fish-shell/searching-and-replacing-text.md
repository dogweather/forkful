---
title:                "Fish Shell: 텍스트 검색과 교체"
simple_title:         "텍스트 검색과 교체"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

텍스트를 찾고 바꾸는 작업을 하는 이유는 간단합니다. 이 작업은 반복적인 작업을 자동화하고 작업을 더 효율적으로 처리하는 데 도움이 됩니다.

## 어떻게

Fish Shell에서 텍스트를 찾고 바꾸는 방법은 다른 쉘 프로그램과 크게 다르지 않습니다. 하지만 Fish Shell에서는 작업을 더 간단하고 편리하게 처리할 수 있도록 많은 기능을 제공하고 있습니다.

```
Fish Shell에서는 아래와 같은 커맨드를 사용하여 텍스트를 찾고 바꿀 수 있습니다:

subs 명령어로 텍스트를 찾아서 원하는 텍스트로 바꿀 수 있습니다. 예를 들어, "subs hello world"라는 명령어를 입력하면 "hello"라는 단어를 "world"로 바꿀 수 있습니다.

global subs 명령어를 사용하면 찾은 모든 텍스트를 한 번에 바꿀 수 있습니다. 예를 들어, "global subs hello world"라는 명령어를 입력하면 파일 내에 있는 모든 "hello"라는 단어를 "world"로 바꿀 수 있습니다.

subs 명령어와 마찬가지로 선택적인 "i" (ignore case: 대소문자 구분 안 함) 옵션을 사용할 수 있습니다. 예를 들어, "global subs -i hello world"라는 명령어를 입력하면 대소문자를 구분하지 않고 찾은 모든 "hello"를 "world"로 바꿀 수 있습니다.

위의 예시들처럼 subs 명령어와 global subs 명령어는 매우 유용하게 사용될 수 있습니다. 하지만 더 많은 옵션을 적용하여 더 복잡한 텍스트를 찾고 바꿀 수도 있습니다. 자세한 내용은 아래 "깊이 파보기" 섹션을 참고하시기 바랍니다.
```

## 깊이 파보기

Fish Shell에서는 subs와 global subs 명령어를 통해 더 많은 옵션을 사용할 수 있습니다. 예를 들어, "-d" (delete: 텍스트를 삭제) 옵션을 사용하여 텍스트를 삭제할 수 있습니다. 또는 "-r" (regex: 정규식을 사용하여 텍스트를 찾음) 옵션을 사용하여 정규식을 활용할 수 있습니다. 

또한 Fish Shell에서는 sed, awk 등 다른 유명한 쉘 프로그램에서 사용하는 방식과 유사하게 작은 스크립트를 사용하여 더 복잡한 작업을 처리할 수 있습니다. 예를 들어, "sed" 명령어를 사용하여 파일 내의 특정 텍스트를 찾고 바꾸는 작업을 한 줄의 간단한 스크립트로 만들 수 있습니다. 자세한 내용은 Fish Shell 공식 문서를 참고하시기 바랍니다.

## 참고하기

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/subs.html)
- [Fish Shell Cookbook](https://github.com/jorgebucaran/fisherouting)
- [Sed Command in Linux](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)