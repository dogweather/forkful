---
title:    "C++: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 왜

프로그래밍을 하는 사람들은 명령 줄 인수에 대해 읽고 이해하는 것이 중요합니다. 명령 줄 인수는 사용자가 프로그램을 실행할 때 입력 할 수있는 추가 정보를 제공해 주므로 프로그램을 더 유연하게 만들 수 있습니다.

# 사용 방법

```C ++
#include <iostream>

int main(int argc, char* argv[]) {
  // "argc" 변수는 입력 된 인수의 수를 나타내며 "argv"는 입력 된 인수의 배열을 나타냅니다.
  // 첫 번째 인수 (index 0)는 프로그램 이름 입니다.
  // 이어지는 인수는 매개 변수로 사용할 수 있습니다.
  // 예시:
  std::cout << "프로그램 이름: " << argv[0] << "\n";
  for (int i = 1; i < argc; i++) {
    std::cout << "매개 변수 " << i << ": " << argv[i] << "\n";
  }
}

```

입력 할 때 "myprog hello world"와 같이 매개 변수를 추가하면 다음과 같은 출력이 표시됩니다 :
```
프로그램 이름: myprog
매개 변수 1: hello
매개 변수 2: world
```

# 깊게 파고들기

위에서 언급했듯이, `argc` 변수는 입력 된 인수의 수를 나타내고 `argv` 배열은 실제 인수를 저장합니다. `wchar_t*` 타입인 `GetCommandLine` 함수를 사용하면 명령 줄 전체를 가져와 프로그램에서 나눌 수 있습니다.

# 참고

- [C++ 참고 문서](https://docs.microsoft.com/en-us/cpp/cpp/command-line-arguments?view=vs-2019)
- [C++ 표준 라이브러리 참고 문서](https://en.cppreference.com/w/cpp/language/main_function)
- [C++ 명령 줄 인수 관련 질문 및 답변](https://stackoverflow.com/questions/3024197/what-main-argc-and-argv-are-main-function-parameters)