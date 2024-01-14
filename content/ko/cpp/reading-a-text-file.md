---
title:                "C++: 텍스트 파일 읽기"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것에 대해 알고 싶은 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 파일에서 데이터를 추출하거나 저장된 정보를 분석해야 할 수도 있습니다. 또는 프로그래밍에 대한 관심이 있어서 파일을 조작하는 방법을 배우고 싶은 경우일 수도 있습니다. 어떤 이유로든, 텍스트 파일을 읽는 것은 프로그래밍에서 중요한 기술이므로 이 글을 읽는 것은 매우 유익할 것입니다.

## 어떻게
텍스트 파일을 읽는 방법은 간단합니다. 먼저 파일을 열고, 파일에서 데이터를 읽는 코드를 작성해야 합니다. 아래 예제를 살펴보세요.

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
  // 파일 열기
  ifstream file("input.txt");

  // 파일에서 한 줄씩 읽기
  string line;
  while (getline(file, line)) {
    // 읽은 데이터 출력
    cout << line << endl;
  }

  // 파일 닫기
  file.close();

  return 0;
}
```

위의 코드를 실행하면 "input.txt" 파일의 내용을 한 줄씩 읽어서 콘솔에 출력하는 결과를 볼 수 있습니다. 아래는 파일의 내용과 출력 결과입니다.

**input.txt**  
Hello World  
This is a text file  
It contains some data  

**출력 결과**  
Hello World  
This is a text file  
It contains some data  

## 딥 다이브
텍스트 파일을 읽는 방법에 대해 더 깊이 알아보겠습니다. 파일을 열 때 사용할 수 있는 다양한 옵션이 있습니다. 예를 들어, 파일을 읽기만 하는 경우에는 ifstream 대신에 ifstream::in 옵션을 사용할 수 있습니다. 또한 파일을 쓰기 전용으로 열 수도 있습니다. 파일을 열 때 사용할 수 있는 다양한 옵션과 이러한 옵션들의 차이점에 대해 알아보는 것도 좋습니다.

## 더 알아보기
- [C++ 파일 입출력 - 점프 투 C++](https://www.jump-to-cpp.com/cpp-file-io/)
- [C++ ifstream::open() 함수 - 알파코딩](http://alphacoding.tistory.com/entry/C-ifstream-open-%ED%95%A8%EC%88%98)  

## 참고
- Markdown 문법: <https://en.wikipedia.org/wiki/Markdown>
- GitHub Flavored Markdown: <https://guides.github.com/features/mastering-markdown/>
- 이 글의 소스 코드: <https://github.com/example/casual-cpp-korean>