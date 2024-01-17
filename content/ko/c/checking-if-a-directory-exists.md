---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜하는 거죠?

디렉토리가 존재하는지 확인하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 이 작업을 하는 이유는 프로그램이 디렉토리를 사용하기 전에 그 디렉토리가 존재하는지 확인하고 필요한 조치를 취하기 위해서입니다.

## 방법:

```C
#include <stdio.h>
#include <stdbool.h>
#include <sys/stat.h>

bool directoryExists(const char *path) {
    struct stat s;
    return (stat(path, &s) == 0 && S_ISDIR(s.st_mode));
}

int main(void) {
    if (directoryExists("./my_directory")) {
        printf("디렉토리가 존재합니다.");
    } else {
        printf("디렉토리가 존재하지 않습니다.");
    }
    return 0;
}
```

위의 예시 코드는 디렉토리가 존재하는지 확인하는 함수와 해당 함수를 사용하는 예시를 보여줍니다. `sys/stat.h` 헤더 파일에 있는 `stat()` 함수를 사용하여 디렉토리의 속성을 확인하고, `S_ISDIR` 매크로를 이용하여 디렉토리인지 아닌지를 판별합니다.

## 깊이 들어가보기:

디렉토리가 존재하는지 확인하는 기능은 유닉스 운영체제에서 비롯되었습니다. 유닉스에서는 파일이나 디렉토리의 속성을 확인하기 위해 `stat` 명령어를 사용했습니다. 이 명령어는 파일과 디렉토리의 속성을 아웃풋으로 제공하는데, 이 중 디렉토리의 속성을 나타내는 비트가 바로 `S_ISDIR` 매크로와 관련이 있습니다.

또한 `access()` 함수를 사용하여 파일이나 디렉토리에 접근할 수 있는지를 확인할 수도 있습니다. 디렉토리의 존재 여부도 `access()` 함수로 확인할 수 있지만, 이 함수는 보다 광범위한 접근 권한을 확인하기 위한 용도로 사용하는 것이 더 바람직합니다.

디렉토리가 존재하는지 확인하는 기능은 파일 시스템을 다루는 프로그램에서 필수적이므로, 운영체제 별로 조금씩 구현 방식이 다를 수 있습니다. 따라서 위 예제 코드는 유의미한 참고자료가 될 수 있지만, 각 운영체제에 따라서 변경되어야 하는 경우도 있을 수 있음을 유의해야 합니다.

## 더 알아보기:

- [stat() 함수의 매뉴얼 페이지](https://man7.org/linux/man-pages/man2/stat.2.html)
- [access() 함수의 매뉴얼 페이지](https://man7.org/linux/man-pages/man2/access.2.html)
- [파일과 디렉토리 속성 확인하기](https://www.ibm.com/docs/ssw_aix_72/filesbasedir/features.htm)