---
title:    "C++: 임시 파일 생성하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

번역:
## 왜

이번 포스트에서는 C++에서 임시 파일(temporary file)을 생성하는 방법을 살펴보겠습니다. 임시 파일은 프로그래밍에서 자주 사용되며, 애플리케이션을 실행하는 동안 임시적으로 필요한 파일을 만들고 사용한 후 삭제하는 기능을 수행합니다.

## 어떻게 하나요

### 임시 파일 생성하기

임시 파일을 생성하기 위해서는 `<cstdio>` 라이브러리에 정의된 `tmpnam()` 함수를 사용할 수 있습니다. 아래의 코드는 `tmpnam()` 함수를 사용해 임시 파일을 생성하는 간단한 예제입니다.

```C++
#include <cstdio>

int main() {
  // 임시 파일의 이름을 저장할 변수 선언
  char tempFilename[L_tmpnam];
  
  // 임시 파일 생성
  // 첫 번째 매개변수는 이름을 저장할 문자 배열의 주소를 전달합니다.
  // 두 번째 매개변수는 임시 파일의 이름을 작성할 수 있는 버퍼의 크기를 전달합니다.
  // 함수가 성공적으로 실행되면 임시 파일의 이름이 tempFilename 변수에 저장됩니다.
  tmpnam(tempFilename);
  
  // 임시 파일의 이름 출력
  printf("임시 파일 이름: %s\n", tempFilename);
  
  return 0;
}
```

아래는 위 코드의 실행 결과입니다.

```
임시 파일 이름: C:\Users\User\AppData\Local\Temp\fileTDxBwL
```

### 임시 파일 삭제하기

임시 파일을 사용한 후에는 삭제해주어야 합니다. 이를 위해 `<cstdio>` 라이브러리에 정의된 `remove()` 함수를 사용할 수 있습니다. 아래의 코드는 위에서 생성한 임시 파일을 삭제하는 예제입니다.

```C++
#include <cstdio>

int main() {
  // 임시 파일의 이름을 저장할 변수 선언
  char tempFilename[L_tmpnam];
  
  // 임시 파일 생성
  tmpnam(tempFilename);
  
  // 임시 파일 삭제
  // 매개변수로 임시 파일의 이름을 전달합니다.
  remove(tempFilename);
  
  return 0;
}
```

## 깊게 들어가기

임시 파일 생성 및 삭제 작업은 프로그래밍에서 자주 사용됩니다. 이를 효율적으로 수행하기 위해 다양한 방법들이 존재합니다. `tmpnam()` 함수 외에도 C++에서 임시 파일을 생성하는 다른 방법들을 살펴볼 수 있습니다. 예를 들어 `<iostream>` 라이브러리에 정의된 `tmpfile()` 함수를 사용해 임시 파일을 생성할 수 있습니다.

## 더 알아보기

- [C++에서 임시 파일 생성하기](https://www.tutorialspoint.com/creating-temporary-file-using-iostream-library-in-cplusplus)
- [C++ 표준 라이브러리: 임시 파일 생성하기](https://en.cppreference.com/w/cpp/filesystem/temporary_file)
- [C++ 임시 파일 사용 예제 코드](https://www.geeksforgeeks.org/c-programming-create-temporary-file-name/)