---
title:                "C: 임시 파일 생성하기"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 프로그래밍이 필요한 이유는 매우 다양합니다. 일반적으로 프로그램이 실행되는 동안 사용자 데이터를 임시로 저장하거나 프로그램이 실행 중인 동안 생성된 데이터를 파일에 저장하는 데 사용됩니다.

## 어떻게

임시 파일을 생성하는 것은 C 언어에서 매우 간단합니다. 우리는 `tmpfile()` 함수를 사용하여 임시 파일 포인터를 얻을 수 있습니다. 그리고 `fprintf()` 함수를 사용하여 파일에 데이터를 작성할 수 있습니다. 예제 코드는 다음과 같습니다:

```C
#include <stdio.h>

int main() {
    FILE *fp;
    char data[] = "This is a sample temporary file.";

    fp = tmpfile(); // 임시 파일 포인터 획득
    fprintf(fp, "%s\n", data); // 데이터 작성
    fclose(fp); // 파일 닫기

    return 0;
}
```

위 코드를 `tmpfile_example.c` 파일로 저장하고 컴파일한 뒤 실행하면 다음과 같은 결과를 얻을 수 있습니다:

```
$ gcc tmpfile_example.c -o tmpfile_example
$ ./tmpfile_example

$ cat /tmp/tmpfileXXXXXX
This is a sample temporary file.
```

위 예제 코드에서 `tmpfile()` 함수는 `/tmp` 디렉토리에 새로운 임시 파일을 생성하고 해당 파일의 파일 포인터를 반환합니다. 파일 포인터를 사용하여 `fprintf()` 함수를 호출하면 해당 임시 파일에 `data` 문자열을 작성할 수 있습니다. 마지막으로 파일을 닫아서 `fclose()` 함수를 호출합니다.

## 깊이 파고들기

`tmpfile()` 함수는 실제로 운영체제의 기능을 사용하여 임시 파일을 생성합니다. 많은 운영체제에서는 `/tmp` 디렉토리에 생성된 임시 파일이 적절하게 관리되어 해당 파일이 더 이상 필요하지 않을 경우 삭제됩니다. 따라서 임시 파일을 생성하고 데이터를 작성하는 것 외에 파일 관리에 대한 걱정은 없어도 됩니다.

그러나 `tmpfile()` 함수는 사용자가 파괴하지 않는 한 해당 파일을 유지합니다. 따라서 파일을 더 이상 사용하지 않을 때에는 `fclose()` 함수를 호출하여 파일을 닫아야 합니다. 그렇지 않으면 파일이 유지되며 디스크 공간을 차지하게 될 수 있습니다.

## 관련 자료

더 많은 정보를 원한다면 다음의 링크를 참고하세요:

- [Linux tmpfile() 함수 문서](https://www.man7.org/linux/man-pages/man3/tmpfile.3.html)
- [C 파일 입출력 관련 기초](https://modoocode.com/81)
- [C 언어 기초 강좌](https://opentutorials.org/course/1335)