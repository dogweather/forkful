---
title:                "명령줄 인수 읽기"
html_title:           "C: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

*왜* 누군가가 명령 줄 매개 변수를 읽는 것에 참여할지 설명해보겠습니다. 명령 줄 매개 변수는 C 프로그램에서 사용자에게 입력을 요청하는 것보다 더 유연한 방식입니다. 이를 통해 사용자는 실행 시에 다양한 매개 변수를 제공하고 원하는 동작을 만들 수 있습니다.

## 방법

다음은 C 프로그램에서 명령 줄 매개 변수를 읽는 방법을 보여주는 예제입니다. 코드 블록은 "```C ...```" 형식으로 제공됩니다.

```
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("입력된 매개 변수의 개수: %d\n", argc);

  for(int i = 0; i < argc; i++) {
    printf("%d 번째 매개 변수: %s\n", i, argv[i]);
  }

  return 0;
}
```

아래는 프로그램을 실행할 때 다양한 매개 변수를 제공하고 출력 결과를 확인하는 방법입니다.

```
$ ./command_line_arguments hello world 123
입력된 매개 변수의 개수: 4
0 번째 매개 변수: ./command_line_arguments
1 번째 매개 변수: hello
2 번째 매개 변수: world
3 번째 매개 변수: 123
```

## 깊이 탐구

C 프로그램에서는 `argc`와 `argv` 변수를 사용하여 명령 줄 매개 변수를 읽습니다. `argc`는 입력된 매개 변수의 개수를 나타내는 정수이고, `argv`는 매개 변수를 저장하는 문자열 배열입니다. `argv` 배열의 첫 번째 요소는 프로그램의 이름을 포함하고, 이후 요소들은 사용자가 제공한 매개 변수를 순서대로 나타냅니다.

See Also
[Command Line Arguments in C Programming](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
[Using Command Line Arguments in C/C++](https://www.thegeekstuff.com/2012/05/c-cpp-sample-commands/)