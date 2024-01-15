---
title:                "디렉토리의 존재 여부 확인"
html_title:           "Bash: 디렉토리의 존재 여부 확인"
simple_title:         "디렉토리의 존재 여부 확인"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요한지 알고 있나요? 그렇다면 이 글을 따라서 배워보세요.

## 어떻게 하나요?

```Bash
if [ -d ~/Documents ]; then
  echo "Documents 디렉토리가 존재합니다."
else
  echo "Documents 디렉토리가 존재하지 않습니다."
fi
```

위의 예시 코드는 `~/Documents` 디렉토리가 존재하는지를 확인하는 코드입니다. 디렉토리가 존재한다면 "Documents 디렉토리가 존재합니다."를 출력하고, 존재하지 않는다면 "Documents 디렉토리가 존재하지 않습니다."를 출력합니다. 이 코드를 통해 디렉토리가 존재하는지를 간단하게 확인할 수 있습니다.

## 깊이 파고들기

이제 조금 더 자세히 알아보겠습니다. `if [ -d <디렉토리 이름> ]; then` 부분에서 `[-d]`는 파일의 유형을 나타내는 플래그입니다. `-d` 플래그는 디렉토리를 나타내며, 디렉토리가 존재하는지를 확인하는데 사용됩니다. 만약 파일을 확인하고 싶다면 `[ -f <파일 이름> ]; then`을 사용하면 됩니다.

## 참고

[Linux Hint - Checking if a File Exists in Bash](https://linuxhint.com/bash_check_if_file_exists/)
[Linuxize - How to Check if a Directory Exists in Bash](https://linuxize.com/post/bash-check-if-directory-exists/)