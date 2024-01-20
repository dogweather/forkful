---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
커맨드 라인 인자 읽기는 프로그램이 사용자 입력을 받는 한 가지 기법입니다. 프로그래머들은 매개변수를 여러가지로 사용하거나, 프로그램 행동을 변경하도록 할 수 있게 해줍니다.

## 어떻게?
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  for(int i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  return 0;
}
```
이 코드의 출력은 다음과 같습니다.
```C
> ./program Foo Bar
> Argument 0: ./program
> Argument 1: Foo
> Argument 2: Bar
```

## 깊이있게 알아보기
일반적인 `main()` 함수의 시그니쳐인 `int main(int argc, char *argv[])`는 UNIX 시스템의 초기 부터 있었습니다. 이것의 대안으로는 환경 변수에 접근하는 것이 있습니다. `char *envp[]`를 `main()` 함수의 세 번째 인자로 추가하는 것으로 가능합니다.
`argc`는 'argument count'의 약자로, 커맨드 라인 인자의 수를 나타냅니다. `argv[]`는 'argument vector'의 약자로, 인자들의 리스트를 나타냅니다.

## 참고자료
1. [Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
2. [C - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
3. [How Command Line Parameters Are Parsed](https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw)