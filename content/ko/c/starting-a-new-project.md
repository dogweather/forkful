---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 & 왜? 
새 프로젝트를 시작한다는 것은 새로운 아이디어나 문제를 해결하기 위한 소프트웨어를 처음부터 개발하는 것을 말합니다. 프로그래머들은 학습, 기술 향상, 비즈니스 문제 해결, 혹은 단순한 창조력 표현을 위해 이를 수행합니다.

## 어떻게:
새로운 C 프로젝트를 시작하는 가장 기본적인 방법을 보겠습니다.

```C
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```
이 코드를 실행하면 다음과 같은 출력을 확인할 수 있습니다:
```C
Hello, World!
```

## 딥다이브
1. 이스토리컬 콘텍스트: C언어는 1972년에 벨 연구소에서 개발되었습니다. 이후로 많은 애플리케이션 및 시스템 프로그래밍 언어의 기반으로 사용되었습니다.
2. 대체제: C언어를 대신할 수 있는 다양한 프로그래밍 언어가 있습니다. Java, Python, C++, Rust 등이 있으며, 각 언어는 특정 문제를 해결하는 데 있어 유용성을 가지고 있습니다.
3. 구현 세부사항: C언어 프로젝트는 보통 컴파일러를 통해 실행 가능한 바이너리를 생성합니다. 이 프로세스는 소스 코드, 컴파일, 링크 등 여러 단계로 구성됩니다.

## 참고 자료
2. 이집트의 프로그래머인 Joel Spolsky가 쓴 ["스택 오버플로우: 가이드"] (https://www.amazon.com/Joel-Software-Occasionally-Developers-Designers/dp/1430209879)도 유익한 참고자료가 될 수 있습니다.