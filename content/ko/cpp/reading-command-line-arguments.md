---
title:                "C++: 컴퓨터 프로그래밍 설명서 : 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍 설명서 : 명령 줄 인수 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인자를 읽는 방법에 대해 배우는 이유는 프로그래밍에서 필수적인 기술이기 때문입니다. 커맨드 라인 인자를 사용하면 사용자가 프로그램을 실행할 때 입력하는 인자를 읽고 그에 따라 프로그램의 동작을 조정할 수 있습니다.

## 방법
커맨드 라인 인자를 읽는 방법은 다양합니다. 가장 간단한 방법은 `main()` 함수의 매개변수로 문자열 배열 `argv`를 받는 것입니다. 이 배열에는 프로그램 실행 시 입력된 모든 인자가 저장됩니다. 아래 예제 코드는 사용자가 입력한 인자를 출력하는 간단한 프로그램입니다.

```C++
#include <iostream>

int main(int argc, char *argv[]) {
	for (int i = 0; i < argc; i++) {
		std::cout << argv[i] << std::endl;
	}
	return 0;
}
```

이 프로그램을 실행하기 위해서는 커맨드 라인에서 `./program arg1 arg2 arg3`와 같이 입력하면 됩니다. 그러면 `arg1`, `arg2`, `arg3`가 각각 출력될 것입니다.

## 심층 분석
커맨드 라인 인자를 읽는 방법에는 더 많은 세부적인 기능들이 있습니다. 예를 들어, `getopt()` 함수를 사용하면 옵션을 포함한 인자를 더 쉽게 처리할 수 있습니다. 또한 `argc`와 `argv`를 사용하는 것 이외에도 다른 방식으로 커맨드 라인 인자를 읽는 방법도 존재합니다. 각각의 방법들에 대해서는 더 많은 학습이 필요합니다.

## 관련 링크
- [argc and argv 입력 인자에 대한 자세한 설명](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [getopt() 함수 사용법](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [커맨드 라인 인자를 처리하는 다양한 방법들](https://en.cppreference.com/w/cpp/utility/program/main)