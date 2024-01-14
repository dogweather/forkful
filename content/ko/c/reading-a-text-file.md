---
title:                "C: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 법을 알고 싶다면 C 프로그래밍 언어에 대한 이해도가 높아야 합니다. 그러므로 기초적인 C 프로그래밍 지식이 있는 분들에게 이 글을 추천합니다.

## 어떻게

텍스트 파일을 읽는 방법에 대해 알아보겠습니다. 먼저, fopen() 함수를 사용하여 파일을 열어야 합니다. 그 후, fgets() 함수를 사용하여 파일에서 각 줄을 읽는 것이 가능합니다. 아래 코드 예제를 참고해보세요.

```C
#include <stdio.h> // 표준 입출력 라이브러리를 포함합니다.

int main() {
  // 파일을 읽기 모드로 엽니다.
  FILE* file = fopen("example.txt", "r");

  // 파일에서 각 줄을 읽습니다.
  char line[100];
  while (fgets(line, sizeof(line), file) != NULL) {
    printf("%s", line); // 각 줄을 출력합니다.
  }

  // 파일을 닫습니다.
  fclose(file);
  return 0;
}
```

위 코드를 실행하면, "example.txt" 파일에 있는 내용이 한 줄씩 출력될 것입니다. 즉, 텍스트 파일을 한 줄씩 읽어오는 것이 가능합니다.

## 딥 다이브

텍스트 파일을 읽는 방법에 대해 더 깊이 알아보겠습니다. fgets() 함수를 사용할 때, 파일의 끝을 나타내는 EOF(end of file) 값을 확인해주어야 합니다. 또한, 파일을 열기 전에 fopen() 함수가 제대로 동작했는지 확인하는 것도 중요합니다. 파일 입출력과 관련된 더 자세한 내용은 다른 자료를 참고해보시기 바랍니다.

## 또 다른 자료들

- [C 파일 입출력](https://modoocode.com/153)
- [C 프로그래밍 입문서](https://ko.wikipedia.org/wiki/C_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8%EC%9D%98_%EC%9E%85%EB%AC%B8)
- [C언어 기초 강좌](http://tcpschool.com/c/intro)