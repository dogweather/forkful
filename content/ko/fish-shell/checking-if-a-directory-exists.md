---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:56:11.567126-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
디렉토리 존재 여부를 확인하는 것은 특정 경로에 폴더가 있는지 없는지 검사하는 과정입니다. 프로그래머들은 파일 시스템 관련 작업을 수행하기 전에 오류를 방지하고 효율적인 코드 실행을 위해 이를 확인합니다.

## How to: (실행 방법)
Fish Shell에서 디렉토리가 존재하는지 확인하는 법은 간단합니다. 테스트 명령어 `test` 를 사용하거나, 보다 직관적인 `-d` 옵션을 사용할 수 있습니다.

코드 예시:
```Fish Shell
if test -d /some/directory
    echo "Exists!"
else
    echo "Does not exist!"
end
```

또 다른 방법:
```Fish Shell
if [ -d /another/directory ]
    echo "Exists!"
else
    echo "Does not exist!"
end
```

실행 결과:
```
Exists!
```
또는
```
Does not exist!
```

## Deep Dive (심층 분석)
디렉토리 존재 여부는 초기 유닉스 시스템에서부터 필요한 기능이었습니다. Fish Shell은 현대적인 쉘으로 간결함과 스크립트의 가독성을 중요시 여깁니다. 전통적인 UNIX `test` 명령어가 `-d` 옵션을 사용하여 디렉토리 존재 여부를 검사하는 방식을 그대로 이어받았지만, 더 읽기 쉽고 사용하기 쉬운 문법을 추가했습니다.

`test` 명령어에 대한 대안으로는 `stat` 또는 `find` 명령어를 사용할 수 있으나, 이들은 추가적인 옵션을 필요로 하고 복잡할 수 있습니다. Fish의 경우 대괄호(`[ ]`)를 사용한 표현이 가능하지만, 이는 POSIX 규약에 따른 것으로, Fish만의 독특한 기능이라기보다는 보편적인 쉘 스크립팅에 가까운 방식입니다.

구현 세부 사항으로는, `-d` 옵션은 디렉토리의 존재뿐만 아니라 디렉토리인지의 여부까지 검사합니다. 이는 파일이 아닌 디렉토리에 대한 작업을 할 때 중요한 차이점입니다.

## See Also (참고 자료)
- Fish Shell 공식 문서: https://fishshell.com/docs/current/
- Unix `test` 명령어에 관한 매뉴얼: https://man7.org/linux/man-pages/man1/test.1.html
- POSIX 쉘 스크립트 가이드라인: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
