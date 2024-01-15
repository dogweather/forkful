---
title:                "디렉터리가 존재하는지 확인하는 방법"
html_title:           "C: 디렉터리가 존재하는지 확인하는 방법"
simple_title:         "디렉터리가 존재하는지 확인하는 방법"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지 확인하는 것에 참여하는 이유는 환경 설정이나 파일 관리와 같은 애플리케이션 개발 프로세스에서 중요한 역할을 합니다.

## 방법
C 프로그래밍에서 디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `opendir()` 함수를 사용하는 것입니다. 만약 디렉토리가 존재하지 않으면 해당 함수는 `NULL` 값을 반환합니다. 다음은 이를 코드로 표현한 예시입니다.

```
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir;

    // 존재하는 디렉토리 경로
    dir = opendir("test_dir");
    
    // 디렉토리가 존재하지 않는 경우
    if (dir == NULL) {
        printf("디렉토리가 존재하지 않습니다.\n");
    } else {
        printf("디렉토리가 존재합니다.\n");
        closedir(dir);
    }
    
    return 0;
}
```

위 코드의 출력은 다음과 같습니다.

```
디렉토리가 존재합니다.
```

## 깊게 파헤치기
위의 예시에서 사용한 `opendir()` 함수는 `<dirent.h>` 라이브러리에서 제공합니다. 이 함수는 디렉토리가 존재하지 않는 경우에도 에러를 반환하지 않기 때문에 조금 더 안정적으로 디렉토리를 확인할 수 있습니다. 또한 `mkdir()` 함수를 사용하여 디렉토리를 생성하거나 `stat()` 함수를 사용하여 디렉토리와 관련된 정보를 확인할 수도 있습니다.

## 참고
- [C - Directory Operations](https://www.tutorialspoint.com/cprogramming/c_directory_handling.htm)
- [C opendir() function](https://www.programiz.com/c-programming/library-function/dirent/opendir)