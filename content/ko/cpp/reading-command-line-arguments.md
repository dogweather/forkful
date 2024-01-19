---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜하고 무엇인가?
C++는 커맨드 라인 인수를 읽는 데 사용하며, 이를 통해 사용자는 프로그램에 입력을 제공할 수 있습니다. 이것은 사용자와 프로그램 간의 동적 상호작용을 가능하게 만듭니다.

## 이렇게 해보세요: 
C++에서 커맨드 라인 인수를 읽는 기본 문법에는 두 가지 주요 구성 요소가 있습니다: 메인 함수의 매개 변수 `argc`와 `argv`.

```C++
#include<iostream>
int main(int argc, char* argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << "인수: " << argv[i] << '\n';
    }
    return 0;
}
```
만약 이 프로그램을 `food apple banana`라는 커맨드 라인 인수와 함께 실행한다면, 출력은 다음과 같을 것입니다:
```C++
인수: food
인수: apple
인수: banana
```

## 더 알아보기: 
이 능력은 Unix 시스템에서부터 시작된 것으로, 이제는 우리가 보는 대부분의 이기종 시스템에서 발견됩니다. 커맨드 라인 인수인 argv는 문자열의 배열입니다. argc는 이 배열에 들어 있는 요소의 수를 나타냅니다. 이 방법 외에도 프로그램에 데이터를 전달하는 다른 방법들도 있지만, 이는 C++에서 가장 일반적이고 기본적인 방법입니다.

## 참고 자료: 
- APUE (Advanced Programming in the UNIX Environment): 이 책은 Unix 프로그래밍에 대한 전반적인 내용을 다루고 있습니다.
- [www.cplusplus.com](http://www.cplusplus.com/): 여기에 모든 C++ 주제에 대한 상세한 설명과 예제가 있습니다.
- [stackoverflow.com](https://stackoverflow.com/): 질문이나 문제가 있다면, 이곳이 가장 좋은 자료입니다.