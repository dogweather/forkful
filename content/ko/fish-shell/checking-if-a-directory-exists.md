---
title:                "디렉토리가 존재하는지 확인하는 방법"
html_title:           "Fish Shell: 디렉토리가 존재하는지 확인하는 방법"
simple_title:         "디렉토리가 존재하는지 확인하는 방법"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 확인하는 것은 프로그래머들이 코드를 작성할 때 자주 하는 작업입니다. 디렉토리가 존재하는지 여부를 확인함으로써, 프로그램의 플로우를 제어하고, 오류를 방지하고, 보안을 강화할 수 있기 때문입니다.

## 방법:

```
Fish Shell에서 디렉토리가 존재하는지 확인하는 방법:

if test -d "/my/directory"
    echo "디렉토리가 존재합니다."
else
    echo "디렉토리가 존재하지 않습니다."
end
```

```
Fish Shell에서 디렉토리가 존재하는지 확인하는 방법:

if contains "/my/directory" (command ls)
    echo "디렉토리가 존재합니다."
else
    echo "디렉토리가 존재하지 않습니다."
end
```

## 딥 다이브:

디렉토리가 존재하는지 확인하는 작업은 인터넷이 발전하기 이전인 1970년대부터 컴퓨터 사용자들이 자주 하는 작업 중 하나였습니다. 이 작업을 수행하기 위해 사용되는 명령어는 운영체제마다 다양하지만, 대부분의 경우에 먼저 디렉토리에 접근 권한이 있는지 확인한 후, 접근 권한이 있다면 디렉토리가 존재하는 것으로 판단합니다. Fish Shell에서는 `test` 명령어를 사용하여 디렉토리가 존재하는지 여부를 확인할 수 있으며, `contains` 명령어를 사용하여 현재 디렉토리에 원하는 디렉토리가 포함되어 있는지 여부를 확인할 수 있습니다.

Fish Shell 이외에도 다른 쉘 프로그래밍 언어에서도 디렉토리가 존재하는지 확인하는 방법들이 제공됩니다. 예를 들어, bash에서는 `-d` 옵션을 사용하여 디렉토리가 존재하는지 여부를 확인할 수 있습니다. 또한, 디렉토리를 비롯한 다양한 파일 유형의 존재 여부를 확인하는 명령어인 `stat`도 많이 사용됩니다.

디렉토리가 존재하는지 확인하는 작업은 보안 상의 이유로 자주 사용되기도 합니다. 예를 들어, 파일을 올바른 디렉토리 경로에 저장하는지 확인하기 위해 디렉토리가 존재하는지 여부를 검사할 수 있습니다.

## 관련 정보:

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Linux Shell Tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [쉘 스크립트를 이용한 모범 사례](https://github.com/progit/progit2/blob/master/book/02-git-basics/03-branching-merging-and-remote-repositories.textile)