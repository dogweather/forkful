---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜? (What & Why?)
임시 파일(temporary file) 생성은 일시적으로 데이터를 저장하기 위한 방법입니다. 프로그래머가 이를 활용하면 작업 도중 데이터 손실을 방지하고, 메모리에 여유를 줄 수 있습니다.

## 동작 방법: (How to:)
C에서는 표준 라이브러리 함수인 `tmpfile`을 사용해 임시 파일을 생성할 수 있습니다. 그 예제를 살펴봅시다.

```C
#include <stdio.h>

int main() {
    FILE* tmpf = tmpfile();
    fputs("GeeksForGeeks", tmpf);
    rewind(tmpf);
    char buf[20];
    fgets(buf, sizeof(buf), tmpf);
    puts(buf);

    return 0;
}
```

이 코드를 실행시키면 다음과 같은 결과를 얻을 수 있습니다:

```C
GeeksForGeeks
```

## 깊게 알아보기: (Deep Dive)
`tmpfile` 함수는 1978년에 소개된 ANSI C 표준 라이브러리에 포함되었습니다. 직접 파일을 생성하고 적절하게 관리하는 대신, 이를 사용해 임시 파일을 안전하게 생성합니다. 

대안으로는 `mkstemp`나 `tmpnam` 등의 함수가 있지만, 이들은 잠재적인 보안 문제를 가질 수 있으므로 사용에 주의해야 합니다. `tmpfile`은 파일을 생성하고 자동으로 삭제하는 기능을 제공하며, 이는 프로그램이 비정상적으로 종료되더라도 임시 파일이 시스템에 남지 않도록 돕습니다.

## 참고 자료: (See Also)
- `[C 라이브러리 - tmpfile()]`(https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- `[C언어 : 임시 파일 생성하기]`(https://heecheolman.tistory.com/64)
- `[C언어 tmpfile 함수에 대해 알아봅시다.]`(https://dojang.io/mod/page/view.php?id=386)