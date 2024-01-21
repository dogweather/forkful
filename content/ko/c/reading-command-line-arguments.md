---
title:                "명령줄 인수 읽기"
date:                  2024-01-20T17:55:32.127016-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
명령줄 인자를 읽는 것은 사용자가 프로그램을 실행할 때 주는 입력 값들을 처리하는 것입니다. 이를 통해 프로그램은 더 다양하고 동적으로 작업을 할 수 있습니다.

## How to: (어떻게 하나요?)
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("프로그램 이름: %s\n", argv[0]);
    printf("인자 개수: %d\n", argc - 1);

    for (int i = 1; i < argc; i++) {
        printf("인자 %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

```bash
$ gcc program.c -o program
$ ./program 사과 바나나 체리
프로그램 이름: ./program
인자 개수: 3
인자 1: 사과
인자 2: 바나나
인자 3: 체리
```

## Deep Dive (심층적 이해)
명령줄 인자는 UNIX의 초기부터 사용되어 온 기능입니다. `argc`는 Argument Count의 약자로, 인자의 수를 나타냅니다. `argv`는 Argument Vector의 약자로, 실제 인자들의 목록을 가리킵니다. 각 인자는 문자열 배열에 저장되며, `argv[0]`은 보통 프로그램 자신의 이름입니다.

기본 구문 외에도, 라이브러리 (예를 들어, GNU의 `getopt`)를 사용하여 복잡한 명령줄 인자를 더 쉽게 처리할 수 있습니다. 환경 변수와 파일 입출력을 활용하는 방법도 있지만, 대화형 인터페이스가 아닌 스크립트나 배치 작업에 주로 명령줄 인자가 활용됩니다.

## See Also (참고 자료)
- GNU C 라이브러리 매뉴얼의 명령줄 인자 부분: https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html
- `getopt` 함수 사용 방법: http://man7.org/linux/man-pages/man3/getopt.3.html
- `getopt` 함수를 사용하는 C 프로그래밍 예제: https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html