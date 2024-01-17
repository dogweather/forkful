---
title:                "컴퓨터 프로그래밍에 관한 기사 제목: 커맨드 라인 인자 읽기"
html_title:           "C++: 컴퓨터 프로그래밍에 관한 기사 제목: 커맨드 라인 인자 읽기"
simple_title:         "컴퓨터 프로그래밍에 관한 기사 제목: 커맨드 라인 인자 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
커맨드 라인 인수를 읽는 것은 프로그래머가 사용자가 실행하는 프로그램에 특정한 변수 또는 설정을 제공할 수 있도록하는 것입니다. 대개 프로그램이 시작될 때 주어진 인수를 읽어와 실행에 대한 추가적인 정보를 제공합니다. 이를 통해 프로그램이 다양한 경우에 대응할 수 있으며 사용자 요구를 충족시킬 수 있게 됩니다.

## 작동 방식:
```
#include <iostream>
  
int main(int argc, char *argv[]) {
  std::cout << "Command line arguments:" << std::endl;
  for (int i = 0; i < argc; i++)
    std::cout << argv[i] << std::endl;
}

```

위의 코드는 인수의 총 개수(argc)와 각 인수의 내용(argv[])를 출력하는 기본적인 예제입니다. 만약 프로그램을 ```./program argument1 argument2 argument3```와 같이 실행하면 아래와 같은 결과가 출력됩니다.

```
Command line arguments:
./program
argument1
argument2
argument3
```

## 깊은 곳까지 들어가보기:
커맨드 라인 인수는 유닉스 시스템에서 많이 사용되어온 개념입니다. 이전에는 명령어에 대한 일부 옵션을 지정하기 위해 사용되었으며 다양한 프로그래밍 언어에서 이 개념을 지원하고 있습니다. 또한 프로그램을 실행하는 동안 사용자의 입력을 요구하지 않고 인수를 통해 프로그램을 제어하는 것은 자동화 작업에 매우 유용합니다. 그 외에도, 다양한 라이브러리를 사용해 커맨드 라인 인수를 읽는 다른 방법들이 존재합니다.

## 관련 자료:
- https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/
- https://opensource.com/article/19/5/how-argument-options-c
- https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html