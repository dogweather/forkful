---
title:                "C: 명령 줄 인수 읽기."
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

우리가 프로그래밍 할 때, 종종 프로그램 실행 시 데이터를 전달하고 싶은 경우가 있습니다. 그러나 이러한 데이터는 코드 내에 하드 코딩되어 있지 않기 때문에 외부에서 가져와야 합니다. 이때, 커맨드 라인 인수를 사용할 수 있습니다. 이 블로그 포스트에서는 이러한 커맨드 라인 인수를 읽는 방법에 대해 알아보겠습니다.

## 사용 방법

커맨드 라인 인수를 읽는 방법은 간단합니다. "argc"와 "argv"라는 두 가지 매개변수를 가진 메인 함수를 사용하면 됩니다. argc는 커맨드 라인에 입력된 인자의 수를 의미합니다. 호스트 운영체제에 따라 달라질 수 있지만, 일반적으로 나머지 인자는 "argv"로 전달됩니다.

아래는 C 언어를 사용하여 간단한 프로그램을 작성한 예시입니다. 코드 내에 "```"로 감싸진 부분은 실제 코드입니다.

```
#include <stdio.h>

int main(int argc, char *argv[]) {
    int i;

    printf("전달된 인수의 수: %d\n", argc);
    for (i = 0; i < argc; i++) {
        printf("인수 %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

위 코드를 "sample.c"라는 파일로 저장하고 컴파일 후 실행하면, 커맨드 라인에 입력한 인자들이 출력됩니다. 예를 들어, "sample.exe hello world"라는 커맨드를 입력하면 아래와 같은 출력을 볼 수 있습니다.

```
전달된 인수의 수: 3
인수 0: sample.exe
인수 1: hello
인수 2: world
```

## 딥 다이브

실제로 커맨드 라인 인수를 읽는 작업은 더 복잡합니다. 이러한 인수는 텍스트로서만 전달되기 때문에 이를 다른 데이터 타입으로 변환하는 작업이 필요합니다. 또한, 예외 상황에 대한 처리 및 인수의 순서에 따라 다른 동작을 하는 경우도 고려해야 합니다.

이러한 세부 사항에 대해서는 더 깊이 공부해야 합니다. 더 깊이 공부하기 위해서는 "getopt" 함수나 "argp" 라이브러리를 참고하시기 바랍니다.

## 더 알아보기

- [커맨드 라인 인수를 읽는 방법](https://modoocode.com/108)
- [C언어 커맨드 라인 인수](https://codescracker.com/cpp/cpp-command-line-arguments.htm)
- [getopt 함수](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [argp 라이브러리](https://www.gnu.org/software/libc/manual/html_node/Argp.html)

## 참고 자료

- [메르 링의 C 프로그래밍](https://www.Linux-mag.com/id/3972/)
- [문제 해결을 위한 프로그래밍, 2판](https://book.naver.com/bookdb/book_detail.nhn?bid=1229799)