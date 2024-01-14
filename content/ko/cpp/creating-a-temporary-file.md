---
title:                "C++: 임시 파일 생성"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜?

임시 파일을 생성하는 것이 왜 필요한지 궁금하신가요? 임시 파일은 컴퓨터 프로그래밍에서 중요한 역할을 합니다. 예를 들어, 프로그램 실행 중에 사용자의 입력을 저장하거나, 복구 가능한 데이터를 저장하는 등 여러 가지 용도로 사용될 수 있습니다. 임시 파일은 프로그램이 동작하는 동안 필요하지만, 그 이후에는 필요하지 않은 데이터를 임시적으로 보관하기에 적합합니다.

## 어떻게?

임시 파일을 생성하기 위해서는 C++의 표준 라이브러리 중 하나인 <code>tmpfile()</code> 함수를 사용하면 됩니다. 아래의 코드를 참고해보세요.

```C++
#include <stdio.h>

int main() {
    // 임시 파일 생성
    FILE* temp = tmpfile();

    // 임시 파일에 문자열 쓰기
    fputs("안녕하세요?", temp);

    // 임시 파일을 읽기 위한 파일 포인터 위치 조정
    rewind(temp);

    // 임시 파일에서 문자열 읽기
    char str[20];
    fgets(str, 20, temp);
    printf("임시 파일에 저장된 문자열: %s", str);
}
```
**출력 결과:**
```
임시 파일에 저장된 문자열: 안녕하세요?
```

## 깊이 파고들기

`tmpfile()` 함수는 실제로는 임시 파일을 생성하는 것이 아니라, 메모리를 할당한 후 그 주소를 가리키는 파일 포인터를 반환합니다. 이렇게 함으로써 파일로 여러 가지 작업을 수행할 수 있게 됩니다. 또한 이 함수는 프로그램이 종료되면 자동으로 임시 파일을 삭제해 줍니다. 따라서 별도의 삭제 작업이 필요하지 않습니다.

이 외에도 C++에서는 임시 파일을 생성하는 다른 방법들이 존재합니다. 다만, 유닉스 강의를 따라가는 경우엔 <code>mkstemp()</code> 함수를 사용하시면 됩니다. 이 함수는 <code>tmpnam()</code> 함수가 재귀적인 방식으로 파일 이름을 생성하는 점을 보완하고 있으며, 보안적인 측면에서 더 좋습니다.

## 더 알아보기

[<code>tmpfile()</code> 함수 설명서](https://modoocode.com/104) \
[<code>mkstemp()</code> 함수 설명서](https://modoocode.com/110) \
[<code>tmpnam()</code> 함수 설명서](https://modoocode.com/105)