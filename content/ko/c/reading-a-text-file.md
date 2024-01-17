---
title:                "텍스트 파일 읽기"
html_title:           "C: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
오늘 우리는 C 프로그램에서 텍스트 파일을 읽는 방법을 배워볼 예정입니다. 텍스트 파일을 읽는 것은 단순하게 말하면, 컴퓨터에 저장된 문자열을 읽어서 화면에 출력하는 것입니다. 프로그래머들은 이 기술을 사용하여 필요한 정보를 읽고 처리할 수 있습니다.

## 어떻게:
이제 실제로 C 코드로 텍스트 파일을 읽어보겠습니다. 아래에 예제 코드와 해당 코드를 실행한 결과를 제시해 드리겠습니다.

```C
#include <stdio.h>
int main(){
    // 텍스트 파일을 읽기 위해 fopen() 함수를 사용합니다.
    // 파일의 경로와 읽기 모드("r")를 지정합니다.
    FILE *fp = fopen("sample.txt", "r");

    // if문을 사용하여 파일을 성공적으로 열았는지 확인합니다.
    if(fp == NULL){
        printf("파일을 열 수 없습니다.");
        return 1;
    }

    // 텍스트 파일의 내용을 한 줄씩 읽어서 출력합니다.
    char line[1000];
    while(fgets(line, 1000, fp) != NULL){
        printf("%s", line);
    }

    // 파일을 닫습니다.
    fclose(fp);

    return 0;
}
```

위 코드를 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
안녕하세요!
이것은 샘플 파일입니다.
텍스트 파일을 읽는 방법을 공부하는 것이 성공하길 바랍니다.
```

## 깊게 들어가기:
C 프로그래밍에서 텍스트 파일을 읽는 것은 매우 중요한 기술입니다. 따라서 중요한 이유 중 하나는 C 프로그래머들이 필수적으로 익혀야 할 기술이라는 것입니다. 더 나아가서, 텍스트 파일을 읽는 것 외에도 다른 방법으로 정보를 읽을 수 있지만, 각각의 방법에는 장단점이 있습니다. 예를 들어, C 프로그래밍에서는 텍스트 파일을 읽는 것보다 바이너리 파일을 읽는 것이 쉽지 않습니다. 따라서 항상 텍스트 파일을 읽는 것을 먼저 배우는 것이 좋습니다.

## 참조:
- [C 프로그래밍 강좌 - 파일 입출력](https://modoocode.com/98)
- [C 프로그래밍 튜토리얼 - 파일 입출력](http://www.c4learn.com/c-programming/c-file-handling/)