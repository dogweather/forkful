---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성은 예측 불가능한 숫자를 만드는 것입니다. 이는 게임, 암호화, 시뮬레이션 등에서 중요한 역할을 합니다.

## 어떻게:

Fish 쉘에서 랜덤한 숫자를 생성하기 위해 `random` 명령어를 사용합니다. 숫자 범위를 지정하려면 두 개의 인수로 최솟값과 최댓값을 지정합니다. 이는 다음과 같이 작동합니다:

```fish
set random_number (random 1 100)
echo $random_number
```
위의 코드를 실행하면 1부터 100 사이의 랜덤 숫자가 화면에 출력됩니다.

## 깊은 이해 

Fish 쉘의 `random` 명령어는 내부적으로 `/dev/urandom`을 사용하여 랜더성을 제공합니다. 이는 표준 Unix/Linux 무작위 원천입니다.
랜덤 숫자 생성의 대안으로는 `/dev/random`, `openssl rand` 등이 있습니다. 이들은 보안 중심의 애플리케이션에서 사용하기에 좋습니다. 
하지만, `random` 명령어는 현실적인 대부분의 사용 사례에서 충분한 랜덤성을 제공합니다.

## 참고 자료

1. [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
2. [Random number generation in Linux](https://www.howtogeek.com/118594/how-the-linux-random-number-generator-works/)
3. [Openssl rand](https://www.openssl.org/docs/man1.1.0/man1/rand.html)
4. [Linux random vs urandom](https://www.2uo.de/myths-about-urandom) 

끝까지 읽어 주셔서 감사합니다. 행복한 코딩 되세요!