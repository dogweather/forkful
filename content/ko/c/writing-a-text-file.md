---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

텍스트 파일 쓰기는 문자 데이터를 파일에 저장하는 과정입니다. 프로그래머는 이를 통해 데이터 로그, 설정, 사용자 입력 결과 등을 보존합니다.

## How to: (방법)

```c
#include <stdio.h>

int main() {
    FILE *fptr;
    fptr = fopen("example.txt", "w");
    
    if(fptr == NULL) {
        printf("파일을 열 수 없습니다.\n");
        return 1;
    }
    
    fprintf(fptr, "안녕하세요, 파일에 글을 씁니다.\n");
    fclose(fptr);
    return 0;
}
```
실행 결과, "example.txt" 파일이 생성되고 "안녕하세요, 파일에 글을 씁니다." 문장이 저장됩니다.


## Deep Dive (심도 있는 정보)

- 초기 C언어에서는 `<stdio.h>` 라이브러리에 정의된 `fopen`, `fprintf`, `fclose` 등의 함수로 텍스트 파일을 썼습니다.
- 대안으로는 POSIX 표준의 `write` 함수 등이 있으나 표준 C로는 `fprintf` 등이 권장됩니다.
- 파일 쓰기 구현은 OS에 따라 달라 파일 시스템 API 사용이 개념적으로 중요합니다.

## See Also (더 보기)

- [C Standard Library - stdio.h](https://en.cppreference.com/w/c/io)
- [Learn C Programming](https://www.learn-c.org/)