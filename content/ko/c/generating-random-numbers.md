---
title:                "랜덤 숫자 생성"
html_title:           "C: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

"
## 무엇 & 왜?
난수를 생성한다는 것은 무엇인지, 프로그래머들이 왜 이 일을 하는지에 대해 두-세 문장으로 설명합니다.

## 방법:
```C
// 0에서 9 사이의 난수 출력하기
int random_number = rand() % 10;
printf("%d", random_number);
```
위 코드는 C 언어를 사용하여 난수를 생성하는 예시입니다. `rand()` 함수를 호출하여 0에서 9 사이의 임의의 숫자를 출력합니다. 

## 깊게 파보기:
1. 역사적 배경: 난수 생성은 컴퓨터 과학의 초기부터 사용되어 온 기법입니다. 1940년대부터 사용되던 "선형 합동법"이 먼저 등장하여 여전히 사용되고 있습니다.
2. 대안: 난수 생성 방법은 다양하지만 가장 흔히 사용되는 기법은 "유사 난수"입니다.
3. 구현 세부사항: C 언어에서 `rand()` 함수는 유사 난수를 생성합니다. 그러나 이는 진정한 난수가 아니며, 시드값에 따라 반복될 수 있는 단점이 있습니다.

## 참고 자료:
- [선형 합동법 정보](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [흔히 사용되는 난수 생성 방법](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)