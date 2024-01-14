---
title:    "C: 디렉터리가 존재하는지 확인하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜
디렉토리가 있는지 확인하는 것에 대해 관심이 있는 분들을 위해 작성되었습니다. 이 기술은 다양한 프로그램에서 중요한 역할을 합니다.

## 방법
우선 <dirent.h>를 include 해주어야 합니다. 그 다음, access()함수를 사용해서 디렉토리의 존재 여부를 확인할 수 있습니다. 아래는 예제 코드와 결과를 담은 코드 블록입니다.

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

int main(void) {
  char dir[100];
  scanf("%s", dir);

  if(access(dir, F_OK) == 0) {
    printf("%s 라는 디렉토리가 존재합니다.", dir);
  } else {
    printf("%s 라는 디렉토리는 존재하지 않습니다.", dir);
  }
  return 0;
}
```

### 출력
```
mydir 라는 디렉토리는 존재하지 않습니다.
```

## 심층적인 내용
access() 함수는 디렉토리 뿐만 아니라 파일의 존재여부를 확인할 수 있습니다. 이 함수에서 사용되는 mode 매개변수는 파일의 권한을 나타내며, F_OK를 입력하면 존재 여부를 확인할 수 있습니다. 또한, opendir() 함수를 사용하면 디렉토리 내의 파일 목록을 가져올 수 있습니다.

## 참고
[access() 함수에 대한 더 자세한 내용](https://www.tutorialspoint.com/c_standard_library/c_function_access.htm)
[opendir() 함수에 대한 더 자세한 내용](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)