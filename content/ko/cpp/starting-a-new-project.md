---
title:                "새 프로젝트 시작하기"
html_title:           "C++: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 왜

새 프로젝트를 시작하게 되는 이유는 다양합니다. 어떤 사람은 새로운 기술을 배우기 위해, 어떤 사람은 자신의 아이디어를 실제로 구현하기 위해, 어떤 사람은 좋은 경험이 될 것 같아서, 그리고 어떤 사람은 그냥 재미있어서일 수도 있습니다.

## 어떻게

새로운 C++ 프로젝트를 시작하는 것은 간단합니다. 우선, 새로운 디렉토리를 만들고, 그 안에 "main.cpp"라는 파일을 만듭니다. 그리고 다음 코드를 입력합니다.

```C++
#include <iostream>

int main() {
    std::cout << "Hello world!" << std::endl;
    return 0;
}
```

위 코드를 입력하고 컴파일하면, "Hello world!"라는 메시지가 출력되는 것을 볼 수 있습니다. 이제 이 코드를 수정하고 원하는 프로젝트를 만들어 나갈 수 있습니다.

## 딥 다이브

새 프로젝트를 시작할 때 주의해야 할 몇 가지 사항이 있습니다. 먼저 어떤 기능을 구현할 것인지, 어떤 라이브러리를 사용할 것인지, 어떤 디자인 패턴을 적용할 것인지 등을 고민해야 합니다. 또한, 코드를 작성하기 전에 잘 설계된 계획을 세운 뒤 차근차근 구현하는 것이 중요합니다. 그리고 처음에는 간단한 기능부터 시작하여 점차 복잡한 기능을 추가하는 것이 좋습니다.

# 참고

- [C++ 참고 문서](https://en.cppreference.com/w/)
- [C++ 새 프로젝트 시작하기](https://docs.microsoft.com/en-us/cpp/build/walkthrough-creating-and-using-a-static-library-cpp?view=msvc-160)