---
title:    "C++: 임시 파일 만들기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

가끔은 우리가 프로그램을 만들다 보면 임시 파일이 필요한 경우가 있습니다. 이러한 임시 파일은 우리가 작업하는 동안 일시적으로 데이터를 저장하거나, 임시적으로 생성된 파일을 사용해야 할 때 유용하게 쓰입니다. 따라서 임시 파일을 생성하는 방법을 배우는 것은 매우 중요합니다.

## 어떻게

우선, C++의 `<fstream>` 헤더 파일을 포함해야 합니다. 그러면 임시 파일을 생성할 수 있는 `tmpfile()` 함수를 사용할 수 있습니다. 아래는 예제 코드와 출력 결과입니다.

```C++
#include <iostream>
#include <fstream>

int main() {
    // 임시 파일 생성
    std::FILE* tempFile = std::tmpfile();

    // 쓰기 모드로 파일 열기
    std::ofstream ofs;
    ofs.open("temp.txt");
  
    // 파일에 쓰기
    ofs << "Temporary file created successfully!" << std::endl;

    // 임시 파일 닫기
    ofs.close();

    // 결과 확인
    std::cout << "Temp file created." << std::endl;
    return 0;
}
```

**결과:**

```
Temp file created.
```

위 코드에서 볼 수 있듯이, 임시 파일을 생성하고 쓰기 모드로 파일을 열어 데이터를 파일에 작성하는 과정을 거칩니다.

## 딥 다이브

위의 예제에서는 `std::tmpfile()` 함수를 사용해 임시 파일을 생성했지만, 더 복잡한 파일 구조를 필요로 한다면 다른 방법을 사용해야 합니다. 예를 들어, `std::ofstream` 대신에 `std::fstream`을 사용해 파일을 열 수도 있습니다. 또한, 임시 파일을 생성할 때 `tmpnam()` 함수를 함께 사용해 파일 이름을 설정할 수도 있습니다.

## 참고

- [C++ <fstream> 헤더 파일 설명서](https://www.cplusplus.com/reference/fstream/)
- [C++ 임시 파일 생성 예제 코드](https://www.geeksforgeeks.org/tmpfile-function-in-c/)
- [C++ tempnam() 함수 설명서](https://www.cplusplus.com/reference/cstdio/tempnam/)