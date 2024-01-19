---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 넘버는 예측 불가능하게 선택된 숫자입니다. 이는 보안, 데이터 분석, 게임 내 무작위 요소 등 다양한 컴퓨터 프로그램에서 필요로 합니다.

## 어떻게:

랜덤 숫자 생성은 Bash에서 아주 간단합니다. `$RANDOM` 환경 변수를 사용하면 됩니다. 예시는 다음과 같습니다.

```Bash
echo $RANDOM
```

출력 예시:

```Bash
13420
```

범위가 주어진 경우 (예. 1부터 50) 아래와 같이 사용할 수 있습니다:

```Bash
echo $((1 + RANDOM % 50))
```

출력 예시:

```Bash
35
```

## 디프다이브:

Bash에서는 `$RANDOM`이라는 내장 환경 변수를 통해 랜덤 넘버를 생성합니다. 이는 POSIX 표준에 따라 32768 (2^15)까지의 범위에서 난수를 제공합니다. 더 큰 범위의 난수가 필요한 경우, 여러번 `$RANDOM`을 호출하거나 다른 도구를 사용해야 합니다. 예를 들어, `/dev/urandom` 또는 `openssl` 같은 리눅스 도구를 사용할 수 있습니다.

## 참고자료:

- Bash Manual : https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- $RANDOM : https://tldp.org/LDP/abs/html/randomvar.html
- How to generate random number in Bash : https://www.cyberciti.biz/faq/bash-shell-script-generating-random-numbers/
- Linux /dev/random file: http://www.linfo.org/dev_random.html
- OpenSSL : https://www.openssl.org/docs/manmaster/man1/rand.html