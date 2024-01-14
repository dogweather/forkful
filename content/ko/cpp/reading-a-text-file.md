---
title:    "C++: 텍스트 파일 읽기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 중요한지 궁금하신가요? C++ 프로그래밍에서 텍스트 파일은 매우 유용한 도구입니다. 텍스트 파일을 읽는다면, 여러분의 프로그램은 사용자 입력을 받거나, 데이터를 읽어오거나, 그리고 어떤 정보를 출력할 수 있게 됩니다.

## 어떻게

아래의 예제 코드들을 통해, 텍스트 파일을 읽는 방법을 살펴보세요. 읽은 내용을 가공하거나, 출력하는 방법에 따라 다른 방식으로 코드를 작성할 수 있습니다.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {

  string line; // 줄 단위로 읽은 내용을 저장할 변수

  // 파일 입력을 위한 ifstream 객체 생성
  ifstream inputFile("example.txt"); 

  // 파일의 끝까지 계속해서 읽어오기
  while (getline(inputFile, line)) { 
    cout << line << endl; // 한 줄씩 출력
  }

  inputFile.close(); // 파일 끝나면 닫아주기

  return 0;
}
```

위의 코드를 실행하면, `example.txt` 파일의 내용이 모두 출력될 것입니다.

```C++
Hello World!
This is an example text file.
```

## Deep Dive

텍스트 파일을 읽는 방법에는 여러 가지가 있을 수 있습니다. 여러분의 프로그램에 맞게 적절한 방식을 선택해보세요. 또한, `ifstream`이외에도 `getline` 외에도 다양한 함수들이 존재합니다. 자세한 정보는 관련 문서를 참조해보세요.

## 친척 글

- [C++ 텍스트 파일 읽고 쓰기] (https://www.programiz.com/cpp-programming/library-function/fstream)
- [C++ 파일 입력과 출력] (https://ko.wikipedia.org/wiki/읽기 및 쓰기)
- [C++ 입출력 예제] (https://www.geeksforgeeks.org/different-ways-to-open-a-text-file-in-c/)