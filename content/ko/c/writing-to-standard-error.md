---
title:                "C: 표준 오류에 쓰는 것"
simple_title:         "표준 오류에 쓰는 것"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

프로그래밍을 하다보면 때때로 오류가 발생할 수 있습니다. 이러한 오류를 찾고 해결하는 과정에서 디버깅은 매우 중요합니다. 이때, 표준 에러 스트림을 활용하면 디버깅에 도움이 될 수 있습니다. 이 블로그 포스트에서는 표준 에러 스트림을 사용하여 디버깅하는 방법에 대해 알아보겠습니다.

## 사용 방법

표준 에러 스트림은 "stderr"이라는 파일 포인터를 사용하여 접근할 수 있습니다. 아래 코드는 "Hello World!"를 표준 에러 스트림에 출력하는 간단한 예시입니다.

```C
#include <stdio.h>

int main()
{
    fprintf(stderr, "Hello World!");
    return 0;
}
```

위 코드를 컴파일하고 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
Hello World!
```

이번에는 표준 에러 스트림을 사용하여 오류 메시지를 출력하는 예시를 살펴보겠습니다.

```C
#include <stdio.h>

int main()
{
    int num = 10;

    if (num < 5)
    {
        printf("Number is less than 5.");
    }
    else
    {
        fprintf(stderr, "Error: Invalid number!");
    }
    return 0;
}
```

위 코드를 컴파일하고 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
Error: Invalid number!
```

위 예시에서 볼 수 있듯이, 표준 에러 스트림을 사용하면 오류 메시지를 출력할 수 있으므로 디버깅에 매우 유용하게 사용할 수 있습니다.

## 더 깊게 알아보기

표준 에러 스트림의 이해를 위해 다음의 사이트들을 참고할 수 있습니다.

- [C Programming Tutorial: The Standard Error Stream](https://www.learn-c.org/en/The_Standard_Error_Stream)
- [C - File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Understanding "stderr", "stdout" and "stdin" in C](https://stackoverflow.com/questions/7099995/stderr-stdout-stdin-in-c)

## 더 참고하기

- [C 프로그래밍 개발 환경 구축하기](https://www.itworld.co.kr/news/126096)
- [C 프로그래밍 배우기](https://www.spoqa.com/blog/256/)
- [C 개발자를 위한 유용한 자료 모음](https://business.pinterest.com/ko/blog/resources-for-c-developers)