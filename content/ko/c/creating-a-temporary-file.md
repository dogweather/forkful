---
title:                "C: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

일시적인 파일을 생성하는 것에 대해 궁금하신가요? 이 글에서는 C 프로그래밍에서 가끔 필요한 일시적인 파일을 만드는 방법과 그 의미에 대해 소개합니다.

## 어떻게 하나요?

가장 먼저, 일시적인 파일을 생성하기 위해 `tmpfile()` 함수를 사용합니다. 이는 파일 디스크립터를 리턴하는데, 여기서 우리는 새로운 파일을 생성하지 않는 걸 주목해야 합니다. 새로운 파일을 생성하지 않기 때문에, 우리는 일시적인 파일을 사용한 후에 나중에 지울 필요가 없습니다.

아래의 코드 예제를 살펴보세요:

```C
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *file;
    char c;

    // 일시적인 파일 생성
    file = tmpfile();

    // 사용자 입력 받기
    printf("파일에 저장할 문자를 입력하세요: ");
    c = getchar();

    // 파일에 쓰기
    putc(c, file);

    // 파일 읽어오기
    rewind(file); // 파일 포인터 위치를 처음으로 바꾸기
    c = getc(file);

    // 결과 출력
    printf("입력한 문자는 %c 입니다.\n", c);

    return 0;
}
```

위의 코드를 실행하면, 사용자가 입력한 문자를 일시적인 파일에 저장하고, 다시 그 파일에서 문자를 읽어와 출력합니다. 간단한 예제이지만, 여러분은 이를 응용하여 더 복잡한 로직을 구현할 수 있습니다.

## 깊게 들어가기

일시적인 파일을 생성하는 방법 외에도, 우리는 파일 경로와 이름을 지정하여 직접 파일을 생성해줄 수도 있습니다. 이를 위해 `tmpnam()` 함수를 사용하면 됩니다. 아래의 코드 예제를 살펴보세요:

```C
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *file;
    char tmpname[L_tmpnam];
    char c;

    // 일시적인 파일 경로 생성
    tmpnam(tmpname);

    // 파일 생성 및 열기
    file = fopen(tmpname, "w+");

    // 사용자 입력 받기
    printf("파일에 저장할 문자를 입력하세요: ");
    c = getchar();

    // 파일에 쓰기
    putc(c, file);

    // 파일 읽어오기
    rewind(file); // 파일 포인터 위치를 처음으로 바꾸기
    c = getc(file);

    // 결과 출력
    printf("입력한 문자는 %c 입니다.\n", c);

    return 0;
}
```

위의 코드를 실행하면, 일시적인 파일의 경로를 생성하고 그 경로를 이용해 파일을 생성하고 열어 데이터를 저장하고 읽어옵니다. 이러한 방식은 더 세밀한 제어가 필요한 경우에 유용하게 사용할 수 있습니다.

## 또 다른 정보

일시적인 파일을 생성하는 것은 프로그래밍에서 가끔 필요한 작업이기 때문에, 더 많은 정보를 알고 싶은 분들을 위해 아래의 링크를 참고하세요:

- [The tmpfile() function](http://www.cplusplus.com/reference/cstdio/tmpfile/)
- [The tmpnam() function](http://www.cplusplus.com/reference/cstdio/tmpnam/)

# 더 알아보기

이 글에서는 C 프로그래밍에서 일시적인 파일을 생성하는 방법에 대해 언급했습니다. 하지만 여러분은 이를 활용하여 더 다양한 작업을 수행할 수 있습니다. 따라서 더 많은 지식을 얻기 위해