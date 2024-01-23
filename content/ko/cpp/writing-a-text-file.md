---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
텍스트 파일 쓰기는 데이터를 일반 텍스트 형식으로 저장하는 과정입니다. 프로그래머들은 정보를 영구적으로 저장하고, 나중에 쉽게 접근하거나 데이터를 교환하기 위해 이 과정을 사용합니다.

## How to: (방법:)
```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("example.txt");
    if (outfile.is_open()) {
        outfile << "안녕하세요, 파일에 텍스트를 씁니다!\n";
        outfile << "C++로 파일 작업은 간단해요.";
        outfile.close();
    } else {
        std::cerr << "파일을 열 수 없습니다!" << std::endl;
    }
    return 0;
}
```
```plaintext
안녕하세요, 파일에 텍스트를 씁니다!
C++로 파일 작업은 간단해요.
```

## Deep Dive (심층 분석)
텍스트 파일 작성은 C++ 초기부터 사용됐습니다. fstream 라이브러리는 입출력 작업에 사용되는 표준 방법입니다. C의 FILE*과 stdio.h 대신 사용됩니다. 데이터를 문자열로 썼을 때와 이진 형태로 썼을 때 차이가 있으니, 용도에 맞게 선택해 사용해야 합니다.

## See Also (더 보기)
- [C++ 파일 입출력](http://www.cplusplus.com/doc/tutorial/files/)
- [C++ I/O 스트림 라이브러리](https://www.learncpp.com/cpp-tutorial/input-and-output-io-streams/)
