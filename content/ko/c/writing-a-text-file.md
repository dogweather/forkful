---
title:                "C: 텍스트 파일 작성하기"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

문자 파일을 쓰는 이유는 복잡한 데이터를 저장하고 공유하기 위함입니다.

## 하는 법

아래 코드 블록을 통해 코딩 예제와 출력 결과를 확인할 수 있습니다.

```C
#include <stdio.h>
 
int main() {
    // 파일 포인터 생성
    FILE *fp;
    
    // 파일 열기
    fp = fopen("sample.txt", "w");
    
    // 데이터 쓰기
    fprintf(fp, "여러분, 안녕하세요!");
    
    // 파일 닫기
    fclose(fp);
    
    return 0;
}
```

위 코드는 "sample.txt"라는 파일을 새로 생성하고, 그 안에 "여러분, 안녕하세요!"라는 문장을 쓰는 간단한 예제입니다. 이제 해당 파일을 열어보면 정확히 입력한 문장이 저장되어 있는 것을 확인할 수 있습니다.

## 깊이 파고들기

문자 파일을 쓰는 것은 기본적으로 파일을 생성하고 그 안에 데이터를 쓰는 과정입니다. 이 과정에서 특정 오류에 대한 처리, 파일에 접근하는 방식 등 다양한 요소가 포함될 수 있습니다. 또한 파일을 읽는 방법과 비슷한 구조를 가진다는 점에서 파일 입출력에 대한 개념을 확장하는 데도 도움이 됩니다. 따라서 프로그래밍을 좀 더 깊이 이해하고 싶은 분들은 문자 파일을 쓰는 과정에 대해 더욱 자세히 공부해보시는 것도 좋은 방법입니다.

## See Also

[파일 입출력 기초 in C](https://www.geeksforgeeks.org/basics-file-handling-c/)

[Writing Files in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

[Exploring the C file input/output library](https://www.harding.edu/fmccown/internetexplorer/cio/)

감사합니다!