---
title:                "랜덤 숫자 생성하기"
html_title:           "C++: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 것에 참여하는 이유는 다음과 같습니다. 우선, 데이터 시뮬레이션, 암호화 및 게임 등 다양한 분야에서 무작위성이 필요하기 때문입니다. 이러한 무작위성을 제공하는 랜덤 숫자 생성기는 매우 중요한 역할을 합니다.

## 하는 방법
랜덤 숫자를 생성하는 가장 간단한 방법은 `rand()` 함수를 사용하는 것입니다. 아래의 코드 블록을 참고해주세요.

```C++
// 임의의 범위에서 랜덤 숫자 생성하기
int num = rand() % 100; // 0부터 99까지의 랜덤 숫자 생성

// 특정 범위에서 랜덤 숫자 생성하기
int min = 10; // 최소값 설정
int max = 20; // 최대값 설정
int num = min + rand() % (max - min + 1); // 10부터 20까지의 숫자 중 랜덤하게 생성

// srand() 함수를 통해 시드값 설정하기
srand(time(0)); // 실행할 때마다 다른 시드값 설정
int num = rand() % 10; // 0부터 9까지의 랜덤 숫자 생성
```

출력 예시:

```
54
15
8
```

## 디플 다이브
랜덤 숫자 생성기의 핵심은 시드(seed)값입니다. 시드값은 랜덤 숫자의 기반이 되는 값으로, 이 값이 변하면 생성되는 랜덤 숫자도 다르게 됩니다. 따라서 랜덤 숫자를 생성하기 전에 시드값을 설정하는 것은 매우 중요합니다. 또한, `rand()` 함수는 실제로는 난수를 생성하는 것이 아니라 유사 난수(pseudo-random)를 생성하는 것입니다. 따라서 완전한 무작위성을 원한다면 외부 라이브러리를 사용해야 합니다.

## 참고
1. [C++ 문서 - 랜덤 함수](http://www.cplusplus.com/reference/cstdlib/rand/)
2. [랜덤 숫자 생성기의 시드값 설정하기](https://docs.microsoft.com/ko-kr/cpp/c-runtime-library/reference/srand-srand)
3. [난수 발생기의 시드(seed)값 설정하는법은?](https://coderanch.com/t/335862/java/-)
4. [유사 난수 - 위키백과](https://ko.wikipedia.org/wiki/%EC%9C%A0%EC%82%AC_%EB%82%9C%EC%88%98)