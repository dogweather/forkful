---
title:                "커맨드 라인 인수 읽기"
html_title:           "C: 커맨드 라인 인수 읽기"
simple_title:         "커맨드 라인 인수 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
커맨드 라인 인수를 읽는 것은 프로그래머가 사용자로부터 정보를 받을 수 있는 간단하고 효율적인 방법입니다. 이를 통해 사용자는 프로그램 실행 시 추가적인 설정을 할 수 있고, 프로그래머는 이를 활용해 유연하고 다양한 기능을 추가할 수 있습니다.

## 사용 방법:
```c
// 예시를 통해 커맨드 라인 인수를 읽는 방법을 알아보겠습니다.
#include <stdio.h>

int main(int argc, char *argv[]) {
  // 프로그램의 실행 파일 이름 출력
  printf("프로그램 이름: %s\n", argv[0]);
  
  // 커맨드 라인 인수 출력
  for (int i = 1; i < argc; i++) {
    printf("%d번째 인수: %s\n", i, argv[i]);
  }
  
  return 0;
}
```
커맨드 라인 인수와 관련된 다양한 기능들이 있지만, 여기서는 간단한 예시를 통해 기본적인 사용 방법을 알아보았습니다. 위 코드를 실행하면, 프로그램의 이름과 함께 추가로 입력한 인수가 순서대로 출력됩니다.

## 깊이 들어가보기:
커맨드 라인 인수를 읽는 기능은 이제는 프로그래밍에서 필수적인 부분이 되었습니다. 이는 이전에는 사용자와 상호작용하기 위한 중요한 수단이었습니다. 현재는 예전에 비해 덜 중요하긴 하지만, 여전히 다양한 프로그래밍 언어와 환경에서 사용되고 있습니다. 또한 커맨드 라인 인수 외에도 프로그램에서 사용자로부터 정보를 받을 수 있는 다양한 방법들이 있지만, 커맨드 라인 인수는 여전히 가장 간단하고 직접적인 방법입니다. 커맨드 라인 인수의 작동 방식은 프로그램 라이브러리에 따라 다를 수 있지만, 대부분의 경우 ```argc```와 ```argv[]``` 이 두 변수를 통해 커맨드 라인 인수를 읽습니다.

## 관련 자료:
- [C 언어 공식 문서](https://ko.wikipedia.org/wiki/%EC%96%B8%EC%96%B4_(%ED%8C%8C%EC%9D%BC))
- [커맨드 라인 인수 사용 예제](https://codeforwin.org/2018/01/c-program-to-read-command-line-arguments.html)
- [커맨드 라인 인수와 프로그래밍 언어의 발전 과정](https://www.freecodecamp.org/news/how-i-started-programming-with-command-line-arguments-a3efc1885c4f/)