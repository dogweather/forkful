---
title:                "임시 파일 만들기"
html_title:           "C++: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

C++ 프로그램을 사용하다보면 때때로 임시 파일(temporary file)의 생성이 필요한 순간이 있습니다. 임시 파일은 일시적인 데이터를 저장하고 관리하는 데 유용합니다. 이럴 때, 어떻게 임시 파일을 생성할 수 있는지 알아보겠습니다.

## 어떻게

```C++
#include <iostream>
#include <fstream>

int main() {
    // 임시 파일을 생성할 디렉토리 설정
    std::string directory = "./temp/";
    // 임시 파일의 이름 설정
    std::string filename = "tempfile.txt";

    // 임시 파일 생성
    std::ofstream tempFile;
    // 디렉토리와 파일 이름을 조합하여 완전한 경로 설정
    tempFile.open(directory + filename);
    if (tempFile.is_open()) {
        // 임시 파일이 열렸는지 확인
        std::cout << "임시 파일이 생성되었습니다";
        // 임시 파일 작업 수행
        // ...

        // 임시 파일 삭제
        tempFile.close();
        remove((directory + filename).c_str());
        std::cout << "임시 파일이 삭제되었습니다.";
    } else {
        std::cout << "임시 파일 생성 실패";
    }

    return 0;
}
```

**출력:**
임시 파일이 생성되었습니다.
임시 파일이 삭제되었습니다.

## Deep Dive

C++에서 임시 파일을 생성하기 위해서는 `<fstream>` 헤더 파일을 사용해야 합니다. `std::ofstream` 클래스를 이용하여 파일을 생성하고 작업을 수행한 후 `close()`를 통해 파일 작업을 마무리 할 수 있습니다. 임시 파일을 삭제하기 위해서는 `remove()` 함수를 사용해야 합니다. 다만, 이 함수는 C 표준 라이브러리에 포함된 함수이기 때문에 C++17 버전부터는 `<cstdio>` 헤더 파일을 이용해야 합니다.

## See Also / 관련 링크들

- [C++ Reference: std::ofstream](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- [C++ Reference: remove()](https://en.cppreference.com/w/cpp/io/c/remove)
- [C++ 표준 라이브러리 레퍼런스](https://cppreference.com)