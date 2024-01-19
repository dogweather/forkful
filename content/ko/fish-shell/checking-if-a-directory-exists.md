---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 확인하는 것은, 파일 시스템에서 특정 디렉토리의 존재 유무를 판단하는 것을 의미합니다. 프로그래머들이 이를 수행하는 이유는, 파일 작업을 실행하기 전에 에러를 방지하거나, 특정 폴더가 존재하는지에 따라 다른 동작을 수행하기 위함입니다. 

## 어떻게:

다음과 같이 샘플 코드를 통해 디렉토리 존재 확인을 수행할 수 있습니다. 

```Fish Shell
if test -d /path/to/directory
   echo "Directory exists"
else
   echo "Directory does not exist"
end
```

이 코드는 `/path/to/directory` 가 존재하는지 먼저 점검하고, 존재할 경우 "Directory exists"를 출력하며, 그렇지 않으면 "Directory does not exist"를 출력합니다. 

## Deep Dive

디렉토리 존재 판단 방법은 컴퓨팅의 역사와 같이 오래되었습니다. 과거에는 디렉토리를 확인하기 위해 사용자가 직접 파일시스템을 탐색해야 했지만, UNIX 시스템에서 제공하는 `test -d` 라는 빌트인 명령을 이용하여 현대의 쉘 스크립트에서는 이 과정을 쉽게 처리할 수 있게 되었습니다.

다른 방법으로는, `if [ -d "$DIR" ]`와 같이 사용하는 방법이 있습니다. `-d` 옵션은 디렉토리가 존재하는지 확인하는 명령으로, `$DIR`이 디렉토리일 경우 참을 반환합니다. 

## 참고 자료

추가 정보를 위해, 다음 링크를 참조하십시오. 

1. Fish Shell 공식 문서: https://fishshell.com/docs/current/
2. UNIX test 명령 정보: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
3. 디렉토리 존재 확인에 대한 스택오버플로우 토론: https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script