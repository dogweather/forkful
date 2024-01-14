---
title:                "C++: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜 

프로그램을 짤 때 디렉토리가 존재하는지 확인하는 것이 왜 중요한지에 대해 알아봅시다.

## 방법

```C++
#include <iostream>
#include <fstream>
#include <sys/stat.h>
using namespace std;

int main() {
    // 존재하는 디렉토리 경로
    string existing_directory = "/Users/username/Desktop";
    
    // 존재하지 않는 디렉토리 경로
    string non_existing_directory = "/Users/username/FakeDirectory";
    
    // stat() 함수를 사용하여 디렉토리의 존재 여부를 확인
    struct stat directory;
    
    // 존재하는 디렉토리의 경우 0, 존재하지 않는 디렉토리의 경우 -1을 반환
    if (stat(existing_directory.c_str(), &directory) == 0) {
        cout << existing_directory << " 디렉토리가 존재합니다." << endl;
    } else{
        cout << existing_directory << " 디렉토리가 존재하지 않습니다." << endl;
    }
    
    if (stat(non_existing_directory.c_str(), &directory) == 0) {
        cout << non_existing_directory << " 디렉토리가 존재합니다." << endl;
    } else{
        cout << non_existing_directory << " 디렉토리가 존재하지 않습니다." << endl;
    }
    
    return 0;
}
```

### 실행 결과:

```
/Users/username/Desktop 디렉토리가 존재합니다.
/Users/username/FakeDirectory 디렉토리가 존재하지 않습니다.
```

## 깊게 들어가기

프로그램을 짜다보면, 파일을 읽어오거나 저장하기 위해 디렉토리를 생성해야 할 때가 있습니다. 그리고 디렉토리를 생성하기 전에 이미 해당 디렉토리가 존재하는지 확인하는 것이 좋습니다. 이를 통해 프로그램이 불필요하게 중단되는 상황을 방지할 수 있습니다.

디렉토리의 존재 여부를 확인하는 가장 간단한 방법은 stat() 함수를 사용하는 것입니다. 이 함수는 path와 stat 구조체를 인자로 받아 해당 경로의 정보를 stat 구조체에 채우는 역할을 합니다. 만약 디렉토리가 존재하면 0을 반환하고, 존재하지 않는다면 -1을 반환합니다. 따라서 stat() 함수를 통해 디렉토리가 존재하는지 확인할 수 있습니다.

## See Also

- http://www.cplusplus.com/reference/sys/stat/
- https://randu.me/tutorials/2.html
- https://blockdmask.tistory.com/31