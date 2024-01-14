---
title:    "C: 텍스트 파일 읽기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것이 중요한 이유는 데이터를 저장하고 공유할 때 필수적인 기능이기 때문입니다.

## 방법
다음은 C 프로그래밍에서 텍스트 파일을 읽는 예제 코드와 출력을 보여줍니다. 

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // 텍스트 파일을 읽기 모드로 열기
    FILE *file_ptr = fopen("sample.txt", "r"); 
    
    // 파일이 존재하지 않는 경우 오류 메시지 출력
    if (file_ptr == NULL) 
    {
        printf("파일을 찾을 수 없습니다.");
        exit(1);
    }

    // 파일에서 한 줄씩 읽어서 출력
    char line[100];
    while (fgets(line, 100, file_ptr) != NULL) 
    {
        printf("%s", line);
    }

    // 파일 닫기
    fclose(file_ptr); 
    
    return 0;
}
```

### 출력
```
Hello,
This is a sample text file.
Thank you for reading!
```

## 깊이 파고들기
C 프로그래밍에서는 파일 입출력 함수인 `fopen()`과 `fclose()`를 사용하여 텍스트 파일을 열고 닫을 수 있습니다. `fopen()` 함수는 읽기 모드로 파일을 열면서 파일에 대한 포인터를 반환해줍니다. 이후 `fgets()` 함수를 사용하여 파일에서 한 줄씩 읽을 수 있으며, `feof()` 함수를 사용하여 파일의 끝에 도달했는지 체크할 수 있습니다. 파일 작업이 끝나면 `fclose()` 함수를 사용하여 파일을 닫아줘야 합니다.

## 관련 링크
- [C 파일 입출력 함수](https://ko.wikipedia.org/wiki/C_%ED%8C%8C%EC%9D%BC_%EC%9E%85%EC%B6%9C%EB%A0%A5_%ED%95%A8%EC%88%98)
- [파일 입출력 in C](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [C 텍스트 파일 읽기 예제](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)

## 더 보기