---
title:    "C: 랜덤 숫자 생성하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜 

난수 생성은 프로그래밍에서 매우 유용합니다. 예를 들어, 게임에서 랜덤한 이벤트를 만들거나, 암호화에 사용될 수 있습니다. 

## 어떻게 

난수를 생성하려면, C 언어에서 제공하는 시드(seed) 개념을 사용해야 합니다. 시드는 난수 생성에 사용되는 기준 값이며, 이를 사용하여 결과가 매번 다르게 나오도록 할 수 있습니다. 아래는 시드를 사용하여 랜덤한 숫자를 생성하는 간단한 예제입니다.
 
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
 
int main()
{
    // 시간을 사용한 시드 생성
    srand(time(NULL));
  
    // 1부터 10까지 랜덤한 숫자 생성
    int randomNum = rand() % 10 + 1;
    printf("랜덤한 숫자: %d", randomNum);
 
    return 0;
}
```

위 예제에서 `srand()` 함수를 사용하여 시드를 생성한 뒤, `rand()` 함수를 이용하여 랜덤한 숫자를 생성합니다. `rand() % 10 + 1` 부분은 1부터 10까지의 숫자를 반환하는데, 이를 원하는 범위로 조정하여 사용할 수 있습니다. 

## 딥 다이브 

난수 생성 알고리즘에는 여러 가지가 있지만, 특히 가장 많이 사용되는 알고리즘은 선형 합동 생성기(Linear Congruential Generator, LCG)입니다. 이 알고리즘은 다음과 같은 공식으로 구현할 수 있습니다.

Xn+1 = (aXn + c) mod m 

여기서 a, c, m 은 상수이며, Xn 은 n 번째 난수를 의미합니다. 이 알고리즘은 간단하지만, 시드 값을 잘 선택하지 않으면 예측이 가능하고 안전하지 않은 난수를 생성할 수 있기 때문에 신중하게 사용해야 합니다. 

## 참고자료 

1. [난수 생성 알고리즘 (위키백과)](https://ko.wikipedia.org/wiki/난수_생성_알고리즘)
2. [C에서 시드 값을 이용한 난수 생성 (TutorialsTeacher)](https://www.tutorialsteacher.com/c/c-random-number-generation)