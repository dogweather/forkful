---
title:                "텍스트 파일 읽기"
date:                  2024-01-20T17:53:57.203471-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
텍스트 파일 읽기는 파일에서 데이터를 추출하는 과정입니다. 이 과정은 설정 로드, 사용자 데이터 처리 또는 파일의 내용 분석 등을 위해 프로그래머들이 자주 사용합니다.

## How to: (어떻게 하나요?)
```C
#include <stdio.h>

int main() {
    FILE *file;
    char filename[] = "example.txt"; // 파일 이름을 여기에 포함하세요.
    char buffer[255]; // 한 줄씩 읽을 수 있는 버퍼를 생성합니다.

    file = fopen(filename, "r"); // 파일을 읽기 모드로 엽니다.

    if (file == NULL) {
        perror("파일 열기 실패");
        return 1;
    }

    while (fgets(buffer, 255, file)) { // 파일을 한 줄씩 읽습니다.
        printf("%s", buffer);
    }

    fclose(file); // 파일을 닫습니다.
    
    return 0;
}
```
Sample Output:
```
안녕하세요, 이것은 예제 텍스트 파일입니다.
텍스트 파일 읽기를 시연하기 위한 내용입니다.
```

## Deep Dive (심층 분석)
텍스트 파일 읽기는 C 언어가 태동한 1970년대부터 존재했습니다. 당시 파일 입출력은 필수적 기능이었고, `stdio.h` 헤더 안에서 이 기능을 제공했습니다. `fopen`, `fgets`, `fclose` 함수들은 파일 작업을 위한 표준 API입니다. 이 방법 말고도 `fread`, `fscanf` 같은 함수들도 있지만, 텍스트 기반 파일에는 `fgets`가 흔히 쓰입니다. `fgets`는 자동으로 문자열 끝에 NULL을 추가해 안전성을 높여줍니다. 따라서, 버퍼 오버러닝이나 파일 끝 판별 작업이 필요 없습니다.

## See Also (더보기)
- C Standard Library: https://en.cppreference.com/w/c/io
- File I/O in C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- GNU C Library documentation for File I/O: https://www.gnu.org/software/libc/manual/html_node/I_002fO-Primitives.html
