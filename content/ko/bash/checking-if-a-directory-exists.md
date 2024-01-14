---
title:                "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일이나 폴더를 생성하거나 찾을 때, 우리는 종종 해당 디렉토리가 이미 존재하는지 확인해야 합니다. 디렉토리가 존재하는지 여부를 체크하는 작업은 우리가 원하는 작업을 수행하기 전에 필수적인 단계입니다. 그렇기 때문에 이 작업을 수행하는 방법에 대해 알아보고자 합니다.

## 사용 방법

먼저, `test`명령어를 사용하여 디렉토리가 존재하는지 확인하는 방법을 알아보겠습니다. 아래의 코드는 디렉토리의 경로를 지정하고 `test` 명령어를 사용하여 디렉토리가 존재하는지 여부를 확인하는 예제입니다.

```Bash
dirname="/home/user/directory"
if test -d "$dirname"; then
  echo "$dirname 디렉토리는 존재합니다."
else
  echo "$dirname 디렉토리가 존재하지 않습니다."
fi
```

위의 예제는 `test` 명령어를 사용하여 디렉토리가 존재하는지 여부를 확인하는 간단한 방법을 보여줍니다.

또 다른 방법으로는 `ls` 명령어를 사용하여 디렉토리를 나열하고 해당 디렉토리가 존재하는지 확인하는 것입니다. 아래의 코드는 `ls` 명령어를 사용하여 디렉토리를 나열하고 `grep` 명령어를 사용하여 특정 디렉토리가 있는지 여부를 확인하는 예제입니다.

```Bash
directory="directory"
if ls | grep "$directory"; then
  echo "$directory 디렉토리는 존재합니다."
else
  echo "$directory 디렉토리가 존재하지 않습니다."
fi
```

위의 예제는 `ls` 명령어를 사용하여 현재 디렉토리에 존재하는 모든 파일과 디렉토리를 나열하고, `grep` 명령어를 사용하여 해당 디렉토리가 존재하는지 확인하는 방법을 보여줍니다.

## 더 깊게 들어가기

디렉토리가 존재하는지 여부를 확인하는 방법은 여러 가지가 있습니다. 위에서 소개한 `test` 명령어와 `ls` 명령어 외에도 `find` 명령어를 사용하거나 `stat` 명령어를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 더 자세한 내용은 해당 명령어들의 man 페이지를 참고하시면 됩니다.

## 참고 자료

- [Bash 쉘 스크립트 프로그래밍](http://blog.naver.com/PostView.nhn?blogId=ura0714&logNo=50112972681)
- [BASH 실행파일 체크하기](http://kusakari.net/archives/119)
- [Bash Shell Scripting 기초](https://www.guru99.com/introduction-to-shell-scripting.html)

## 참고하기