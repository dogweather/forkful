---
title:                "Bash: 랜덤 숫자 생성"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는 것은 프로그래머에게 매우 중요합니다. 데이터를 다루는 많은 애플리케이션에서 랜덤 숫자를 사용하기 때문입니다. 이를 통해 다양한 경우를 시뮬레이션하거나 테스트할 수 있습니다. 또한 랜덤 숫자를 이용하여 보안과 관련된 작업에서 암호화 키를 생성하는 등 보안 측면에서도 중요합니다.

## 방법

랜덤 숫자를 생성할 때는 Bash의 내장 함수인 $RANDOM를 사용하면 간단합니다. 다음과 같은 코드를 실행하면 랜덤한 숫자가 출력됩니다.

```Bash
echo $RANDOM
```

출력 예시:

```
15706
```

이외에도 Shuf나 Jot 같은 프로그램을 사용하여 더 복잡한 랜덤 숫자를 생성할 수도 있습니다. 예를 들어, 0부터 10까지의 랜덤한 숫자를 생성한다면 다음과 같은 코드를 작성할 수 있습니다.

```Bash
shuf -i 0-10 -n 1
```

출력 예시:

```
7
```

## 깊이 알아보기

$RANDOM 함수는 0부터 32767 사이의 숫자를 생성하기 때문에 보다 큰 숫자가 필요하다면 다른 방법을 사용해야 합니다. 일반적으로 나중에 생성할 숫자를 이용하여 난수를 생성하는 알고리즘을 사용합니다. 이를 "Seed 값"이라고 합니다. Seed 값을 예측하기 어렵게 만들기 위해 현재 시간 등의 랜덤 변수를 사용할 수 있습니다.

## 참고자료

- [Bash 문서](https://www.gnu.org/software/bash/manual/html_node/Shell-Variables.html#Shell-Variables)
- [Shuf 문서](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html#shuf-invocation)
- [Jot 문서](https://www.freebsd.org/cgi/man.cgi?query=jot&sektion=1&manpath=freebsd-release-ports)