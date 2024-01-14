---
title:    "C: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하면서 커맨드 라인 인자를 읽는 방법은 매우 중요합니다. 커맨드 라인 인자를 사용하면 프로그램을 실행할 때 입력으로 값을 전달할 수 있습니다. 이를 통해 다양한 입력 값을 테스트하고 다른 용도로 사용할 수 있습니다.

## 방법

커맨드 라인 인자를 읽는 방법은 매우 간단합니다. 먼저, main 함수의 매개변수로 argc와 argv를 선언해야 합니다. 이 매개변수는 각각 입력 값의 개수와 값들을 포함한 배열입니다. 아래는 간단한 예시 코드입니다.

```C
int main(int argc, char *argv[]) {
    // 입력 값의 개수 출력
    printf("Number of arguments: %d\n", argc);

    // 입력 값들을 순서대로 출력
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

이 코드를 컴파일하고 실행하면 다음과 같은 결과가 나타납니다.

```bash
$ gcc -o arguments arguments.c  # 컴파일
$ ./arguments argument1 argument2 argument3  # 실행
Number of arguments: 4
Argument 0: ./arguments
Argument 1: argument1
Argument 2: argument2
Argument 3: argument3
```

위의 예시 코드에서 보이는 것처럼, argv 배열의 첫 번째 요소는 프로그램의 경로이고, 두 번째 요소부터는 입력된 값들이 순서대로 들어가게 됩니다.

## 깊이 들어가기

커맨드 라인 인자를 읽는 방법은 운영체제에 따라 약간의 차이가 있을 수 있지만, 기본적으로 위에서 보인 예시 코드와 같은 방식으로 사용할 수 있습니다. 또한, 프로그램을 실행할 때 입력값을 다르게 주어서 다양한 결과를 볼 수 있습니다.

파일을 읽거나 다른 기능을 수행하기 위해 입력값을 사용하는 경우 커맨드 라인 인자를 사용하면 매우 유용합니다. 또한, 프로그램을 테스트하고 디버깅하는 데에도 유용하게 사용할 수 있습니다.

## 참고 자료

- [GeeksforGeeks: Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Tutorials Point: Command Line Arguments in C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Programiz: Command Line Arguments in C](https://www.programiz.com/c-programming/c-command-line-arguments)