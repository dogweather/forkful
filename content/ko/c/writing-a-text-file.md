---
title:                "텍스트 파일 쓰기"
html_title:           "C: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜 쓸까요?

텍스트 파일을 쓰는 이유는 여러 가지가 있을 수 있습니다. 가장 일반적인 이유는 데이터를 저장하고 나중에 사용하기 위함입니다. 특히, C 프로그래밍에서는 텍스트 파일을 사용하여 프로그램에 필요한 정보를 저장하고 불러오는 것이 매우 흔합니다.

## 어떻게 쓸까요?

아래 예시 코드를 통해 C 프로그래밍에서 텍스트 파일을 쓰는 방법을 알아보겠습니다.

```C
#include <stdio.h>

int main() {
  // 텍스트 파일 열기
  FILE *fp = fopen("sample.txt", "w");

  // 파일에 쓸 내용
  char *text = "Hello, world!";

  // 파일에 쓰기
  fprintf(fp, "%s", text);

  // 파일 닫기
  fclose(fp);

  return 0;
}
```

위 코드를 실행하면, "sample.txt"라는 파일이 생성되고 그 안에 "Hello, world!"라는 내용이 저장됩니다. 이제 다른 프로그램에서 이 파일을 열어 내용을 읽을 수 있습니다.

## 더 들어가기

위의 예시에서는 간단하게 파일에 쓰기만을 다루었습니다. 하지만 텍스트 파일 쓰기에는 더 다양한 기능들이 존재합니다. 예를 들어, 파일을 열 때 "w" 대신 "a"를 사용하여 파일의 끝에 내용을 추가할 수도 있습니다. 또한, 텍스트 파일을 읽어서 변수에 저장하는 방법도 있습니다. 모든 가능한 기능을 소개하기에는 용량이 너무 커지므로, 관련 링크를 참고하시기 바랍니다.

# 관련 링크

- C 프로그래밍에서 텍스트 파일 저장하기: https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm
- C 파일 입출력 함수: https://www.programiz.com/c-programming/c-file-input-output