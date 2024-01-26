---
title:                "리팩토링"
date:                  2024-01-26T01:17:00.249304-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/refactoring.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩토링은 기존 컴퓨터 코드의 구조를 외부 동작을 변경하지 않고 재구성하는 과정입니다. 프로그래머들은 가독성을 향상시키고, 복잡성을 줄이며, 코드를 더 유지보수하기 쉽고 확장성 있게 만들기 위해 이 작업을 수행합니다. 이는 도로에서 트럭 한 대 분량의 시간과 두통을 절약할 수 있습니다.

## 방법:
코드를 정비해봅시다. 배열에서 정수의 평균을 계산하는 함수가 있다고 상상해보세요. 첫눈에 볼 때, 조금 얽히고 설킨 혼란스러움이 있습니다.

**리팩토링 전:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // for-루프 조건에서 합하기, 아야!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("평균: %f\n", calculateStuff(array, length));

    return 0;
}
```

**리팩토링 후:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("평균: %f\n", calculateAverage(array, length));
    return 0;
}
```
이 간단한 예에서조차, 함수를 분리하는 것이 코드를 더 깨끗하고 유지보수하기 쉽게 만드는 방법을 볼 수 있습니다. 각 함수는 이제 단 하나의 책임을 가지고 있습니다 - 깨끗한 코딩에서의 주요 원칙 중 하나입니다.

## 심층 탐구
"리팩토링"이라는 용어는 90년대 후반, 특히 마틴 파울러의 "리팩토링: 기존 코드의 설계 개선" 책의 출판으로 인기를 얻었습니다. 리팩토링은 버그를 수정하거나 새로운 기능을 추가하는 것을 의미하지 않으며, 오히려 코드의 구조를 개선하는 것에 관한 것입니다.

프로세스를 자동화하는 많은 멋진 리팩토링 도구와 통합 개발 환경(IDE)이 있습니다. C와 C++을 위한 CLion 같은 도구가 있지만, 내부에서 무슨 일이 일어나고 있는지 이해하는 것이 중요합니다.

리팩토링의 대안으로는 (위험하고 종종 불필요한) 처음부터 코드를 다시 쓰거나 (장기적으로 더 많은 비용이 들 수 있는) 기술 부채를 안고 사는 것이 포함될 수 있습니다. 구현 세부 사항은 프로젝트에 따라 다르지만, 일반적인 리팩토링에는 변수의 이름 변경하기, 큰 함수를 작은 함수로 나누기 그리고 마법의 숫자를 명명된 상수로 대체하기 등이 포함됩니다.

또한, DRY(반복하지 마라)와 SOLID 원칙과 같은 패턴들이 리팩토링 여정을 안내할 수 있으며, 테스트하기 쉽고 이해하며 협업하기 좋은 코드 베이스를 추구합니다.

## 또한 보기
리팩토링의 바다에 더 깊이 잠수해보려면 다음을 살펴보세요:

- 마틴 파울러의 홈페이지: https://martinfowler.com/ 리팩토링과 소프트웨어 설계에 관한 다양한 기사와 자원들을 보물처럼 제공합니다.
- Refactoring.com: https://refactoring.com/ 리팩토링 기법의 예시와 카탈로그를 제공합니다.
- "리팩토링" 책: 리팩토링에 대한 완전한 견해를 제공하는, 리팩토링의 바이블로 간주됩니다.
- 로버트 C. 마틴의 "클린 코드: 애자일 소프트웨어 장인 정신" 이해하고 유지보수하기 쉬운 코드를 작성하는 방법에 대해 논의합니다.