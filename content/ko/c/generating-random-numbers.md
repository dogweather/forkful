---
title:                "랜덤 숫자 생성하기"
html_title:           "C: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 것에 참여하는 이유는 다양합니다. 일부는 재미있는 게임 또는 시뮬레이션이 필요할 수 있고 다른 사람들은 통계적인 연구에 필요해서 사용할 수 있습니다. 여러분은 이 기능을 원하는 대로 활용할 수 있습니다.

## 하는법
랜덤 숫자를 생성하는 것은 C 프로그래밍에서 매우 간단한 일입니다. ```rand()``` 함수를 사용하여 기본적인 코드를 작성할 수 있습니다.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // 1부터 10까지의 숫자를 랜덤하게 출력하기
    int randomNumber = rand() % 10 + 1;
    printf("%d", randomNumber);
    return 0;
}
```

이 코드를 실행하면 각 실행마다 1부터 10까지의 숫자 중 하나가 출력됩니다.

### 깊게 들어가기
```rand()``` 함수는 매우 간단하지만 실제로는 랜덤한 수가 아닌 의사 난수를 생성합니다. 이는 주어진 시드를 기반으로 계산된 값에 의해 결정됩니다. 또한 C 프로그래밍에서는 여러 종류의 난수 생성 알고리즘을 사용할 수 있습니다. 따라서 랜덤 숫자를 생성할 때는 어떤 알고리즘을 사용해야 하는지에 대해 더 깊이 알아볼 필요가 있습니다.

## 더 알아보기
- [C언어 랜덤 숫자 생성 방법](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Cstdlib 라이브러리](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [의사 난수와 알고리즘](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)

## 관련 링크
- [C 언어 공식 홈페이지](https://ko.wikipedia.org/wiki/C)
- [C 언어 시작하기](https://www.cs.utexas.edu/users/EWD/ewd03xx/EWD316.PDF)
- [C언어 코딩 스타일 가이드](http://www.chickensmoothie.com/Forum/viewtopic.php?f=30&t=5000330)