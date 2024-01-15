---
title:                "표준 에러로 씌우는 것"
html_title:           "C++: 표준 에러로 씌우는 것"
simple_title:         "표준 에러로 씌우는 것"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

표준 오류로 쓰기를 하는 이유는 프로그램에서 오류 메시지를 보여주기 위해서입니다.

## 어떻게?

코드 상에서 에러 메시지를 표준 오류로 출력하기 위해서는 ```C++ std::cerr``` 함수를 사용해야 합니다. 이 함수는 화면에 표시할 메시지와 함께 사용할 수 있으며, 아래와 같이 샘플 코드를 확인할 수 있습니다.

```C++
#include <iostream>
using namespace std;

int main() {
  int num = 10, div = 0;
  
  if (div == 0) {
    cerr << "에러: 0으로 나눌 수 없습니다." << endl;
  }
  
  return 0;
}

```

위의 코드를 실행하면, 표준 오류로 에러 메시지가 출력되는 것을 확인할 수 있습니다.

## 깊이 들어가기

표준 오류는 프로그램에서 중요한 도구로 사용됩니다. 표준 출력과는 다르게 표준 오류는 버퍼링 되지 않고 바로 출력되기 때문에 프로그램의 오류를 로그로 남기는 데 유용합니다. 또한, 표준 오류를 사용하면 사용자에게 더욱 직관적인 오류 메시지를 제공할 수 있습니다.

## 관련 항목

- [C++ 표준 입출력](https://www.educative.io/blog/c-plus-plus-standard-output)
- [C++ 예외처리](https://www.programiz.com/cpp-programming/exception-handling)