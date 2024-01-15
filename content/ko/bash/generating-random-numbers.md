---
title:                "랜덤 숫자 생성"
html_title:           "Bash: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리는 종종 무작위 숫자를 사용하여 프로그래밍을 할 때가 있습니다. 예를 들어, 게임을 만들거나 무작위로 데이터를 생성하거나 시뮬레이션을 실행할 때 무작위 숫자는 도움이 될 수 있습니다.

## 방법

우선, `shuf` (shuffle의 약어)을 사용하여 무작위 숫자를 얻는 것을 살펴보겠습니다. 아래 예제를 실행해보세요.

```Bash
shuf -i 1-10 -n 1
```

위 명령어는 1부터 10까지의 숫자 중에서 무작위로 하나를 출력합니다. `-i` 옵션은 입력 범위를 지정하고, `-n` 옵션은 출력할 숫자의 개수를 지정합니다. 결과는 매번 실행할 때마다 다르게 나올 것입니다.

이번에는 `random` 명령어를 사용하여 무작위 숫자를 출력하는 방법을 살펴보겠습니다. 아래 예제를 실행해보세요.

```Bash
echo $RANDOM
```

`random` 명령어는 이미 Bash 쉘에서 내장되어 있는 명령어이므로 추가적인 설치가 필요하지 않습니다. 위 명령어를 여러 번 실행하면 매번 다른 숫자가 출력됩니다.

마지막으로, 무작위 숫자를 변수로 저장하는 방법을 살펴보겠습니다.

```Bash
random_number=$RANDOM
echo $random_number
```

위 코드는 `echo` 명령어를 사용하여 변수 `random_number`에 저장된 무작위 숫자를 출력합니다. 변수를 사용하면 나중에 해당 숫자를 다시 사용할 수 있습니다.

## 딥 다이브

무작위 숫자를 생성하는 방법은 컴퓨터에서 매우 중요한 역할을 합니다. 프로그램의 결과를 예측할 수 없게 만드는 데 사용되며, 암호화나 보안 분야에서도 사용됩니다. 다양한 무작위 숫자 생성 알고리즘에 대해 자세히 공부하고, 프로그램에서 어떻게 활용할 수 있는지 알아보는 것이 좋습니다.

## 관련 정보

- [Bash Reference Manual - RANDOM](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#index-RANDOM)
- [The Linux Command Line - Generating Random Numbers](http://linuxcommand.org/lc3_wss0150.php)
- [Random Number Generator in Bash Scripting](https://www.baeldung.com/linux/random-number-generator-in-bash-scripting)