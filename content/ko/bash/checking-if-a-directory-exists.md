---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가요?
디렉토리 존재 확인은 프로그램이 사용자가 정의한 디렉토리가 실제로 다른 파일 시스템에 존재하는지 확인하는 과정입니다. 이를 통해 프로그래머는 프로그램의 오류를 사전에 예방하고, 쓸데없는 작업을 피할 수 있습니다.

## 어떻게 하나요:
Bash에서 디렉토리의 존재 여부를 확인하는 방법은 간단합니다.
```Bash
if [ -d "/path/to/dir" ]; then
  echo "Directory exists"
else
  echo "Directory does not exist"
fi
```
이 코드는 "/path/to/dir"라는 디렉토리가 존재하는지 확인합니다. 존재하면 "Directory exists"라는 메시지를 출력하고, 그렇지 않으면 "Directory does not exist"라는 메시지를 출력합니다.

## 깊이 이해하기:
디렉토리 존재 확인은 파일 시스템이 탄생한 이후로 프로그래밍에서 중요한 개념으로 사용되고 있습니다. Bash에서는 '-d' 플래그를 사용하여 디렉토리의 존재 여부를 확인합니다.

다른 방법으로는 `test` 명령어를 사용하는 것도 있습니다.
```Bash
test -d "/path/to/dir" && echo "Directory exists" || echo "Directory does not exist"
```
이는 `-d` 플래그와 비슷하게 동작하지만, 표현이 다르게 되어있습니다.

## 참고 자료:
디렉토리 존재 확인에 대한 추가 정보는 다음 링크를 참조할 수 있습니다.

1. Bash Manual: https://www.gnu.org/software/bash/manual/bash.html
2. Test Command in Linux: https://linuxize.com/post/test-command-in-linux