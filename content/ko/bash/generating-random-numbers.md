---
title:    "Bash: 랜덤 숫자 생성"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자 생성에 참여하는 이유는 다양합니다. 예를 들어, 게임 개발자는 게임에서 무작위 이벤트를 구현하기 위해 난수가 필요할 수 있습니다. 또한 데이터 분석가는 무작위 샘플링을 통해 데이터를 분석할 수 있습니다. 생각지 못한 숫자를 생성하는 한 가지 방법으로 난수를 사용하는 것입니다.

## 하는 법

난수를 생성하는 가장 간단한 방법은 Bash의 내장 함수인 $RANDOM를 사용하는 것입니다. 이 함수는 0부터 32767 사이의 무작위 정수를 생성합니다. 예를 들어, 아래 코드는 1부터 10 사이의 무작위 정수를 생성합니다.

```Bash 
echo $((RANDOM%10+1))
```

출력 예시:

```
9
```

또 다른 방법으로는 더 복잡한 난수를 생성하는 다양한 여러 함수를 이용하는 것입니다. 예를 들어, /dev/random 또는 /dev/urandom 파일에서 무작위 바이트를 읽고 숫자로 변환하는 방식입니다. 아래 코드는 1부터 100 사이의 난수를 생성합니다.

```Bash
echo $(( $(od -An -N2 -i /dev/random) % 100 +1 ))
```

출력 예시:

```
73
```

## 깊이 파헤치기

난수 생성은 보다 정교한 알고리즘을 사용하여 더 많은 randomness를 구현할 수 있습니다. 이 알고리즘들 중에는 linear congruential generator(LCG)와 Middle-Square Method가 대표적입니다. 또한 난수 생성기의 품질을 측정하기 위해 해당 수열의 사이클과 균등성을 평가하는 통계학적인 방법도 존재합니다. 따라서 난수 생성은 수학적인 이론과 통계학적인 지식이 필요한 분야입니다.

## 관련 자료

- [Bash 쉘 스크립팅](https://wiki.kldp.org/HOWTO/html/Bash-Prog-Intro-HOWTO.html)
- [Bash 내장 변수](https://www.edison-newworld.com/436)
- [Bash 무작위 정수 생성 방법](https://stackoverflow.com/questions/25547909/how-to-generate-random-number-using-bash)