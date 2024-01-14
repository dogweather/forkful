---
title:    "C: 임시 파일 만들기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

임시 파일을 생성하는 것에 대해 1-2문장 정도의 이유를 설명합니다.

임시 파일, 또는 임시 데이터를 사용하여 프로그램을 실행하면, 메모리에 부담이나 속도 저하 없이 데이터를 처리할 수 있습니다.

# 사용 방법

C 언어에서 임시 파일을 생성하는 방법을 예제 코드와 함께 설명합니다.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // 임시 파일 생성을 위한 파일 포인터 선언
    FILE *tmp;

    // 임시 파일 생성
    tmp = tmpfile();
    if (tmp == NULL) {
        printf ("임시 파일을 생성하는데 실패하였습니다.\n");
        exit(1);
    }

    // 임시 파일에 데이터 쓰기
    fputs("Hello, World!", tmp);

    // 임시 파일에서 데이터 읽기
    rewind(tmp);
    char buffer[12];
    fgets(buffer, 12, tmp);
    printf("%s\n", buffer);

    // 임시 파일 삭제
    fclose(tmp);

    return 0;
}
```

위의 예제 코드를 실행하면 "Hello, World!"가 출력되는 것을 볼 수 있습니다.

# 더 깊이 들어가기

임시 파일을 생성하는 과정에서 아래와 같이 다양한 옵션을 설정할 수 있습니다.

- `tmpfile()` 함수를 사용하여 기본적인 임시 파일 생성
- `tmpnam()` 함수를 사용하여 임시 파일의 파일명을 지정할 수 있음
- `tmpfile_s()` 함수를 사용하여 보안적으로 안전한 임시 파일 생성

이외에도 임시 파일의 특정 위치나 크기, 퍼미션 등을 설정할 수 있습니다.

# 관련 링크 보기

[임시 파일 생성 및 사용 방법](https://www.geeksforgeeks.org/tmpfile-tmpnam-and-tmpnam-s-functions-in-c-with-examples/) \
[C 언어 입문자를 위한 무료 온라인 강좌](https://www.coursera.org/learn/c-programming) \
[C 언어 공식 문서](https://en.cppreference.com/w/c)