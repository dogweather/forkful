---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜필요한가요?

텍스트 파일 읽기는 컴퓨터 프로그램이 디스크에서 텍스트 파일을 읽어 데이터를 처리하는 과정입니다. 프로그래머들은 파일에서 입력을 받아와서 데이터를 분석하거나 재사용하기 위해 이를 사용합니다.

## 참조 코드:

C++에서 텍스트 파일을 읽는 가장 쉬운 방법은 ifstream 클래스를 사용하는 것입니다.. 아래의 코드는 이를 보여줍니다.

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt");
    
    if (file.is_open()) {
        std::string line;
        while (getline(file, line)) {
            std::cout << line << std::endl;
        }
        file.close();
    } else {
        std::cerr << "Unable to open file" << std::endl;
    }

    return 0;
}
```

위 코드를 실행하면 "example.txt" 파일의 모든 줄이 순차적으로 출력됩니다.

## 깊게 살펴보기:

1. 이러한 파일 입출력 기능은 C++가 처음 등장한 1985년부터 가지고 있었습니다. 그 이후로 이 기능은 계속해서 개선되어 왔습니다.
2. ifstream 외에도 `fopen`, `fread` 등의 C 스타일 함수를 이용할 수 있습니다. 하지만 이들 함수는 사용하기 복잡하고 오류가 발생하기 쉬우므로 주의가 필요합니다.
3. ifstream 클래스는 내부적으로 파일의 위치를 포인팅하기 위한 포인터와 버퍼 메모리를 사용합니다. 이는 성능을 최적화하고 효율적인 파일 읽기를 가능하게 합니다.

## 참고 자료:

더욱 심층적인 정보를 얻고 싶다면 아래 웹사이트의 정보를 참고하십시오.
1. C++ 파일 입출력에 대한 자세한 설명: http://www.cplusplus.com/doc/tutorial/files/
2. C++ ifstream 클래스: http://www.cplusplus.com/reference/fstream/ifstream/
3. 파일 입출력에 관한 C++ 표준 문서: https://www.iso.org/standard/68564.html