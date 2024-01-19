---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Command line arguments는 프로그램이 시작할 때 커맨드라인에서 받는 입력입니다. 프로그래머들은 이를 통해 프로그램 실행 시 변경 가능한 파라미터를 제공합니다.

## 어떻게 하는가:

Gleam 프로그래밍에서 command line arguments를 읽는 방법은 다음의 예시를 통해 살펴보겠습니다.

```Gleam
import gleam/io

fn main(args: List(String)) {
    case args {
        [] ->
            io.println("No arguments provided")

        [first | rest] ->
            io.println("First argument is " + first)
    }
}
```

게다가, 실행 결과는 이렇게 보일 것입니다.

```Shell
$ gleam run my_program arg1 arg2
First argument is arg1
```

## 깊게 들어가보기:

Command line arguments의 사용은 UNIX 운영체제의 초기 시기에 소프트웨어 개발자들에 의해 도입되었습니다. 이것은 사용자가 터미널에서 프로그램을 실행하면서 동시에 특정 입력을 제공할 수 있도록 해주는 아이디어였습니다.

대안으로는 환경 변수를 사용하는 것이 있습니다. 환경 변수는 또한 프로그램 실행을 알맞게 제어할 수 있을 것입니다.

Gleam에서 command line arguments는 `main` 함수의 `args` 매개변수를 통해 제공됩니다. 이 매개변수는 String의 리스트이며 각각의 String은 command line에서 제공된 인수를 나타냅니다.

## 참고 자료:

- Gleam Documentation: [Command Line Arguments](https://gleam.run/book/tour/loops.html)
- Stackoverflow: [What are command line arguments in programming?](https://stackoverflow.com/questions/21503865/what-are-command-line-arguments-in-programming)
- Command Line Arguments in Unix and Shell Programming: [Environment Variables](https://www.geeksforgeeks.org/environment-variables-unix/)