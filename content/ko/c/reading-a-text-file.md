---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 텍스트 파일 읽기에 대하여

## 무엇이며 왜합니까?
텍스트 파일 읽기란, 프로그램이 텍스트 파일의 내용을 읽어내는 것을 뜻합니다. 프로그래머들이 이를 수행하기 때문에, 정보를 파악하고, 파일의 데이터를 조작하거나 분석할 수 있습니다.

## 어떻게 합니까?
C에서 텍스트 파일을 읽는 기본적인 방법은 `fopen`, `fgets`, `fclose` 함수들을 사용하는 것입니다. 아래의 코드는 "test.txt"라는 파일을 열어서 각 라인을 출력하는 간단한 예제입니다.

```C
#include <stdio.h>

int main() {
    FILE *file = fopen("test.txt", "r");
    char line[256];

    if (file == NULL) {
        printf("File does not exist.\n");
        return 1;
    }

    while (fgets(line, sizeof(line), file)) {
        printf("%s", line);
    }

    fclose(file);
    return 0;
}
```

이 코드를 실행하면 "test.txt" 파일의 모든 라인이 출력됩니다.

## 깊게 알아보기
텍스트 파일을 읽는 기법은 컴퓨팅 이론의 초창기부터 사용되었습니다. C에서 제공하는 `fopen`, `fgets`, `fclose`와 같은 함수들은 Standard I/O 라이브러리에 속해 있으며, 매우 빠르고 효율적인 방법입니다.

다른 방법으로는 `fread`, `read`, `mmap`과 같은 함수들도 있습니다. 이들은 시스템 호출을 이용하기 때문에 더 저수준에서 동작하게 됩니다. 이 함수들은 더 많은 제어권을 주지만, 사용하기가 더 복잡합니다.

텍스트 파일 읽기는 파일 시스템은 물론 메모리, 시스템 콜, 버퍼 등 많은 컴퓨팅 요소와 깊게 연결되어 있습니다.

## 참고자료
- C Standard Library: <https://en.cppreference.com/w/c/io>
- Understanding and using C pointers: <https://www.oreilly.com/library/view/understanding-and-using/9781449344184/>
- The Linux Programming Interface: <https://man7.org/tlpi/>