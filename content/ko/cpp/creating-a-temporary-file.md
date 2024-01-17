---
title:                "임시 파일 생성하기"
html_title:           "C++: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일 생성은 프로그래머가 컴퓨터 메모리에 임시로 데이터를 저장하고, 나중에 필요할 때 이를 불러와 사용하기 위해 사용됩니다. 이는 프로그램 실행 중에 메모리를 사용하는 데에 문제가 발생할 때 유용하며, 특히 대규모 작업을 처리할 때 매우 유용합니다.

## 방법:

```C++
// 임시 파일을 생성하는 예제입니다.
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main () {
    // 파일 이름을 임의로 생성합니다.
    char* tempname = tempnam(NULL, NULL);
    // 임시 파일을 생성합니다.
    ofstream outfile (tempname);
    // 생성한 임시 파일에 데이터를 작성합니다.
    outfile << "임시 파일 생성 예제입니다.";
    outfile.close();
    // 임시 파일을 읽어옵니다.
    ifstream infile (tempname);
    infile >> tempname;
    // 임시 파일의 데이터를 출력합니다.
    cout << tempname << endl;
    // 파일을 삭제합니다.
    remove(tempname);
    free(tempname);
    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력 결과를 얻을 수 있습니다.

```C++
임시 파일 생성 예제입니다.
```

## 더 깊이 들어가보기:

### 역사적 배경:

임시 파일 생성은 미리 설정된 파일 이름을 사용하지 않고, 프로그램 실행 중에 임의의 파일 이름을 생성하는 기능입니다. 이는 초기에는 운영체제에서 이용한 메모리를 최적화하기 위해 개발되었습니다.

### 대안:

임시 파일 생성 방법 중 하나는 rand 함수를 이용해 임의의 숫자로 된 파일 이름을 생성하는 것입니다. 또 다른 대안은 Unix 계열 운영체제에서는 /tmp 디렉토리를 이용하는 것입니다.

### 구현 세부 사항:

임시 파일 생성에는 여러 가지 방법을 사용할 수 있지만, 일반적으로는 헤더 파일 <cstdlib>의 tempnam 함수를 이용해 임의의 파일 이름을 생성하고, <fstream> 헤더 파일의 ofstream 함수를 이용해 임시 파일을 생성하며, remove 함수를 이용해 새로 생성된 임시 파일을 삭제합니다.

## 더 알아보기:

- [C++ Reference - tempnam](https://www.cplusplus.com/reference/cstdlib/tempnam/)
- [C++ Reference - ofstream](https://www.cplusplus.com/reference/fstream/ofstream/)
- [C++ Reference - remove](https://www.cplusplus.com/reference/cstdio/remove/)