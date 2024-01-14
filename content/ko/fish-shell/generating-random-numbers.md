---
title:    "Fish Shell: 랜덤 숫자 생성"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

난수를 생성하는 것은 프로그래밍에서 일상적인 일입니다. 우리는 무작위의 값을 사용하여 다양한 알고리즘을 테스트하고, 게임에서 랜덤 문제를 만들고, 데이터를 랜덤하게 샘플링하는 등 다양한 목적으로 난수를 사용합니다. 오늘은 Fish Shell에서 난수를 생성하는 방법을 알아보겠습니다.

## 어떻게

우선 `random` 명령어를 사용하여 랜덤한 정수를 생성할 수 있습니다. 예를 들어, 다음 명령어는 1부터 10까지의 정수를 랜덤하게 출력합니다.

```Fish Shell
random -l 1 10
```

출력 예시:

```Fish Shell
6
```

또는 `random` 명령어를 사용할 때 `seq` 명령어를 함께 사용하여 원하는 개수의 난수를 생성할 수도 있습니다. 예를 들어, 다음 명령어는 1부터 10까지의 정수 중 5개를 랜덤하게 출력합니다.

```Fish Shell
seq 5 | xargs random -l 1 10
```

출력 예시:

```Fish Shell
10
7
2
8
5
```

## 심층 탐구

Fish Shell에서 `random` 명령어는 `/dev/random`과 `/dev/urandom` 디바이스를 사용하여 난수를 생성합니다. 이들 디바이스는 컴퓨터의 시스템 엔트로피를 사용하여 랜덤한 데이터를 생성합니다. 따라서 시스템 엔트로피가 부족할 경우 더 낮은 품질의 난수를 생성할 수 있습니다. 이를 방지하기 위해 `/dev/urandom` 디바이스를 사용하여 일정한 시스템 엔트로피 수준을 유지할 수 있습니다.

## 관련 링크

- [Fish Shell 공식 홈페이지](https://fishshell.com)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 랜덤 난수 생성 예제](https://stackoverflow.com/questions/3239018/random-six-digit-number-with-fish-shell)