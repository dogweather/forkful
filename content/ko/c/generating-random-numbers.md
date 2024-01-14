---
title:                "C: 랜덤 숫자 생성하기"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
컴퓨터 프로그램을 작성하다보면 프로그램의 출력물이 매번 똑같고 예측 가능하다는 것을 알게 됩니다. 이때 무작위로 값이 생성되는 것이 중요한 경우가 있습니다. 예를 들어 게임에서 적들이 나타나는 위치를 랜덤하게 설정한다거나, 시뮬레이션 프로그램에서 다양한 조건의 결과를 얻을 때 등 다양한 상황에서 난수를 사용할 수 있습니다. 그렇기 때문에 프로그래머들은 무작위의 숫자를 생성하는 방법을 반드시 알아야 합니다.

## 어떻게
랜덤한 숫자를 생성하는 방법에는 여러 가지가 있지만, 우리는 여기서 C 프로그래밍 언어를 사용하여 난수를 생성하는 방법을 알아보겠습니다. 아래 코드 블록을 참고하면서 따라해보세요.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 현재 시간을 기반으로 시드(seed) 생성
    srand(time(0));
    
    // 1부터 10까지의 무작위 정수를 출력하는 예제
    int num = rand() % 10 + 1;
    printf("%d\n", num);
    
    // 1부터 100까지의 무작위 실수를 출력하는 예제
    float fnum = (float)rand() / RAND_MAX * 100;
    printf("%.2f\n", fnum);
    
    // 특정 범위 내의 임의의 정수를 출력하는 예제
    int min = 20, max = 30;
    int rand_num = rand() % (max - min + 1) + min;
    printf("%d\n", rand_num);
    
    return 0;
}
```

위 코드에서 ```srand(time(0))``` 함수를 사용하면 현재 시간을 기반으로 무작위 값을 생성할 때 사용하는 시드(seed)를 설정할 수 있습니다. 시드를 설정하지 않으면 프로그램을 실행할 때마다 같은 난수가 생성되므로 주의해야 합니다.

## 깊이 들어가기
C 프로그래밍 언어에서는 ```rand()``` 함수를 사용하여 난수를 생성할 수 있습니다. 이 함수는 0부터 ```RAND_MAX``` 값 사이의 무작위 정수를 반환합니다. 하지만 원하는 범위 내의 값을 출력하려면 적절한 연산이 필요합니다. 위 코드에서는 나머지 연산과 곱셈을 사용하여 특정 범위 내의 정수와 실수를 출력하는 방법을 보여주었습니다. 또 다른 방법으로는 난수 생성을 위한 라이브러리를 사용하는 것이 있습니다. 예를 들어 ```<random>``` 헤더 파일에는 다양한 난수 생성 함수가 포함되어 있으며 보다 쉽게 사용할 수 있습니다.

## 관련 자료
- [C 프로그래밍의 입문자를 위한 난수 생성 가이드](https://m.blog.naver.com/wideeyed/221295038008)
- [rand 함수 사용 예제](https://dojang.io/mod/page/view.php?id=439)
- [연속적인 난수 생성을 위한 <random> 헤더 파일](https://modoocode.com/263)

---

## 참고 자료
- [난수 생성과 관련된 다양한 프로그래밍 언어의 차이점](https://www.quora.com/What-are-the-differences-in-random-number-generation-between-different-programming-languages)