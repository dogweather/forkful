---
title:                "텍스트 파일 읽기"
html_title:           "C++: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 무엇과 왜 하나요?
텍스트 파일을 읽는 것은 단순한 텍스트 문서의 내용을 메모리에 로드하는 것을 의미합니다. 프로그래머는 이 작업을 수행하는 이유는 메모리에 로드한 데이터를 프로그램에서 사용할 수 있기 때문입니다.

# 방법:
아래의 ```C++ ... ``` 코드 블록 안에 코딩 예제와 샘플 출력을 제공합니다.

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
    ifstream input_file("input.txt"); // input.txt 파일 열기
    string content; // 파일 내용을 저장할 변수 생성

    if (input_file.is_open()) { // 파일이 열렸는지 확인
        while (getline(input_file, content)) { // 파일 내용을 한 줄씩 읽어오기
            cout << content << endl; // 파일 내용 출력
        }
    }
    else {
        cout << "파일을 열 수 없습니다." << endl; // 파일을 여는데 실패한 경우 에러 메시지 출력
    }

    input_file.close(); // 파일 닫기
    return 0;
}
```

# 깊게 들어가보세요:
(1) 읽기 전용으로 사용된 이산식 추가 언어의 첫 번째 버전은 1962년에 나온 JOVIAL 이었습니다. 오늘날, 다양한 종류의 텍스트 파일을 읽기 위해 C++이 주로 사용되고 있습니다. (2) 파일을 읽는 대안으로는 바이너리 파일을 읽는 것이 있습니다. 이 경우, 파일의 내용이 이진수로 저장되어 있으므로 이를 해석하기 위한 추가 작업이 필요합니다. (3) C++에서 파일을 읽는 방법에는 fgetc(), fgets(), fscanf() 등이 있습니다. 위의 코드에서는 getline()을 사용하였습니다.

# 관련 자료:
- [C++ 파일 입출력 레퍼런스](http://www.cplusplus.com/reference/fstream/)
- [C++ 파일 읽기 및 쓰기 튜토리얼](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ 파일 입출력으로 프로그램 데이터 관리하기](https://modoocode.com/271)