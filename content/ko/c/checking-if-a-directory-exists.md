---
title:                "디렉토리의 존재 여부 확인하기"
date:                  2024-01-19
html_title:           "Arduino: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉터리 존재 여부 확인은 파일 시스템에서 특정 경로의 폴더가 실제로 있는지 확인하는 작업입니다. 프로그래머들은 리소스가 적절한 위치에 있는지 확인하거나 에러를 방지하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
C에서는 `<sys/stat.h>` 헤더의 `stat` 함수를 이용해 디렉터리 존재 여부를 확인할 수 있습니다. 간단한 예로:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat statbuf;
    char *dirname = "example_dir";
    
    // stat 함수로 디렉터리 정보 확인
    if (stat(dirname, &statbuf) == 0 && S_ISDIR(statbuf.st_mode)) {
        printf("Directory '%s' exists.\n", dirname);
    } else {
        printf("Directory '%s' does not exist.\n", dirname);
    }
    
    return 0;
}
```

실행 결과:

```
Directory 'example_dir' exists.
```
또는

```
Directory 'example_dir' does not exist.
```

## Deep Dive (심층 분석)
`stat` 함수는 Unix에서 오래 전부터 사용되어왔습니다. 파일의 메타데이터를 얻기 위해 사용되며, 여기에는 파일 타입도 포함됩니다. `stat` 구조체의 `st_mode` 필드를 이용하면 디렉터리인지 아닌지 구분할 수 있습니다. 하지만 `stat`은 파일 시스템에 접근해야 하므로, 자주 호출하면 성능에 영향을 줄 수 있습니다. 

대안으로 `opendir`과 `readdir` 같은 함수를 사용할 수도 있지만, 이들은 파일 내용을 읽기 위한 것으로, 단순히 존재 여부만 확인하려면 `stat`가 더 직접적입니다.

C11 표준 이후 최신 C에서는 파일 시스템을 위한 표준 라이브러리가 개선되었습니다. 하지만, 현재 많은 시스템들이 여전히 POSIX API를 따르므로, `stat` 함수 사용법을 알아두는 것이 편할 수 있습니다.

## See Also (더 보기)
- [POSIX stat](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/sys/stat.h.html)
- [C11 Standard](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)
- [Linux Manual Page for stat(2)](https://man7.org/linux/man-pages/man2/stat.2.html)
