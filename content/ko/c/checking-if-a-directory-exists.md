---
title:    "C: 디렉토리가 존재하는지 확인하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍에서 디렉토리가 존재하는지 확인하는 것을 왜 해야 할까요? 보통 운영 체제에서 파일 시스템을 다루는 작업을 할때, 디렉토리가 존재하는지 여부를 확인하고 그에 따른 동작을 취해야 할 필요가 있습니다.

## 해야할 방법

C 프로그래밍에서 디렉토리 존재 여부를 확인하는 방법은 간단합니다. `#include <stdio.h>`와 `#include <dirent.h>` 두 개의 헤더 파일을 포함해 줍니다. 그리고 다음과 같은 코드를 작성합니다.

```
DIR *dir = opendir("directory_name");
if(dir != NULL) {
  printf("Directory exists");
  closedir(dir);
}
else {
  printf("Directory does not exist");
}
```

위의 코드에서 `opendir()` 함수는 디렉토리가 존재하는 경우 디렉토리 스트립터를 반환하고, 존재하지 않는 경우 NULL 값을 반환합니다. 따라서 if-else 문으로 디렉토리의 존재 여부를 확인할 수 있습니다.

## 깊게 파고드는 법

위에서 소개한 방법은 가장 기본적인 디렉토리 존재 여부를 확인하는 방법입니다. 그러나 보다 정확한 예외 처리를 위해서는 `stat()` 함수를 사용하는 것이 더 좋습니다. `stat()` 함수는 파일의 상태를 검사할 수 있는 함수로, 파일의 종류, 크기, 수정 시간 등 다양한 정보를 제공합니다. 따라서 이를 통해 디렉토리인지 아닌지를 더 정확하게 판단할 수 있습니다.

```
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>

struct stat st;
if(stat("directory_name", &st) == 0) {
  if(S_ISDIR(st.st_mode)) {
    printf("Directory exists");
  }
  else {
    printf("Not a directory");
  }
}
else {
  printf("Directory does not exist");
}
```

위의 코드에서 `stat()` 함수는 0을 반환할 경우 파일의 상태 정보를 구조체 `st`에 담습니다. 그리고 `S_ISDIR()` 매크로를 통해 디렉토리인지 여부를 확인할 수 있습니다.

## 참고 자료

- [opendir() function](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [stat() function](https://www.tutorialspoint.com/unix_system_calls/stat.htm)
- [S_ISDIR() macro](https://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html)