---
title:                "코드를 함수로 구성하기"
aliases:
- /ko/cpp/organizing-code-into-functions/
date:                  2024-01-26T01:10:10.769265-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
코드를 함수로 분할한다는 것은 코드를 작고 재사용 가능한 조각으로 나누는 것을 의미합니다. 우리는 반복을 피하고, 코드의 가독성을 높이며, 디버깅과 테스팅을 단순화하기 위해 이렇게 합니다. 잘 조직된 함수들은 사용하고 공유할 준비가 된, 깔끔하게 라벨이 붙은 도구 상자를 가진 것과 같을 수 있습니다.

## 방법:
보편적인 작업인 원의 면적 계산을 예로 들어보겠습니다. 매번 같은 수식을 쓰는 대신, 함수로 그것을 캡슐화합니다.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "반지름이 " << r << "인 원의 면적은 " << calculateCircleArea(r) << "입니다" << std::endl;
    return 0;
}
```

출력 예시:
```
반지름이 5인 원의 면적은 78.5397입니다
```

## 심층 분석
역사적으로, 절차와 함수는 1960년대에 이전의 명령형 프로그래밍 언어에서 "스파게티 코드" 문제를 해결하기 위해 옹호된 구조적 프로그래밍의 근간이었습니다. OOP(객체 지향 프로그래밍)와 같은 대안들은 이러한 함수들을 데이터 구조와 연관시켜 더욱 발전시킵니다. C++에서는 일반 함수, 클래스 메소드(정적 메소드 포함), 람다, 템플릿 함수 등이 있으며, 각각 다른 이점을 제공합니다. 잘 조직된 함수를 구현하는 것은 일반적으로 DRY("Don't Repeat Yourself", "자신을 반복하지 마세요")와 SRP(단일 책임 원칙, Single Responsibility Principle)와 같은 원칙을 준수하는 것을 포함하며, 이는 각 함수가 한 가지 일만 하고 그것을 잘하도록 하는 것을 의미합니다.

## 참고
C++의 함수에 대한 더 자세한 정보:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

함수와 관련된 설계 원칙:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

람다와 고급 함수 사용에 대해 알아보기:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
