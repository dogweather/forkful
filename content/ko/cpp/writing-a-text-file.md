---
title:    "C++: 텍스트 파일 작성하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것은 프로그래밍에서 중요한 역할을 합니다. 파일 작성을 통해 다양한 작업을 수행할 수 있어서 많은 프로그래머들이 이를 사용합니다.

## 사용 방법

텍스트 파일을 작성하는 방법에는 여러 가지가 있습니다. 여기서는 C++ 언어를 통해 간단한 예제를 보여드리겠습니다. 코드 블록 안의 명령어를 실행하면 새로운 텍스트 파일이 생성됩니다.

```C++
#include <fstream>
using namespace std;

int main(){
  //새로 만들 파일의 경로와 이름을 설정합니다.
  ofstream file("example.txt");

  //파일에 내용을 쓰기 위해 << 연산자를 사용합니다.
  file << "안녕하세요! 이것은 텍스트 파일의 첫 번째 줄입니다." << endl;
  file << "텍스트 파일은 프로그래밍에서 매우 중요한 역할을 합니다." << endl;

  //파일을 닫습니다.
  file.close();

  return 0;
}
```

위 코드를 실행하게 되면 "example.txt"라는 새로운 파일이 생성되며, 파일 안에는 "안녕하세요! 이것은 텍스트 파일의 첫 번째 줄입니다."와 "텍스트 파일은 프로그래밍에서 매우 중요한 역할을 합니다."라는 문장이 적힌 것을 확인할 수 있습니다.

## 깊게 들어가기

텍스트 파일을 생성하는 방법 외에도, 이미 존재하는 파일에 내용을 추가하거나 수정할 수도 있습니다. 이를 위해서는 파일을 열 때 어떤 모드로 열지를 지정해주어야 합니다. 예를 들어, "example.txt"라는 파일이 이미 존재한다고 가정하고, 아래 코드를 실행하면 해당 파일에 내용이 추가됩니다.

```C++
#include <fstream>
using namespace std;

int main(){
  //이미 존재하는 파일에 내용을 추가할 때는 아래와 같이 실행합니다.
  ofstream file("example.txt", ios_base::app);

  //파일에 새로운 내용을 추가합니다.
  file << "텍스트 파일을 사용하는 또 다른 예시입니다." << endl;

  //파일을 닫습니다.
  file.close();

  return 0;
}
```

따라서, 텍스트 파일을 생성하여 단순히 내용을 작성하는 것 뿐만 아니라 기존 파일에 내용을 추가하거나 수정하는 등 다양한 작업을 할 수 있습니다.

## 참고 자료

- [C++ 에서 텍스트 파일 작성하기](https://modoocode.com/203)
- [C++ 파일 입출력(iostream) - 파일 만들기](https://blockdmask.tistory.com/361)
- [파일 입출력 모드별 설명](https://blockdmask.tistory.com/364)