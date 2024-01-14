---
title:                "C: 디렉토리 존재 여부 확인하기"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

bestOfCoder -
## 왜

프로그램을 작성하다보면 가끔은 특정 디렉토리가 존재하는지 확인해야 하는 상황이 생기기도 합니다. 이를 위해서는 디렉토리가 존재하는지 확인하는 기능이 필요합니다.

## 어떻게

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("./example_directory");

    if (dir) {
        // 디렉토리가 존재하는 경우
        printf("디렉토리가 존재합니다!");
        closedir(dir);
    } else {
        // 디렉토리가 존재하지 않는 경우
        printf("디렉토리가 존재하지 않습니다!");
    }
    return 0;
}
```
```
디렉토리가 존재합니다!
```

위 코드는 디렉토리가 존재하는지 확인하는 간단한 예시입니다. `opendir` 함수를 통해 해당 경로에 디렉토리를 열고, `if` 문을 통해 디렉토리가 존재하는지 확인합니다. 만약 디렉토리가 존재하지 않으면 `else` 문이 실행됩니다. 마지막으로 `closedir` 함수를 통해 열었던 디렉토리를 닫아줍니다.

## 딥 다이브

디렉토리가 존재하는지 확인하는 방법에는 여러가지가 있습니다. 위 코드에서 사용한 `opendir` 함수를 대신하여 `access` 함수를 사용할 수도 있습니다. `access` 함수는 파일에 대한 접근 권한이 있는지 확인하는데 사용되며, 디렉토리가 존재하는지 여부도 확인할 수 있습니다.

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    if(access("./example_directory", F_OK) == 0) {
        // 디렉토리가 존재하는 경우
        printf("디렉토리가 존재합니다!");
    } else {
        // 디렉토리가 존재하지 않는 경우
        printf("디렉토리가 존재하지 않습니다!");
    }
    return 0;
}
```

## 참고 자료

- [C언어 코딩하기 - 입문편](https://www.oss.kr/learn_and_share/lectures/467)
- [Access Function in C programming language](https://www.geeksforgeeks.org/access-function-in-c-programming-language/)
- [C Directory Handling](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)