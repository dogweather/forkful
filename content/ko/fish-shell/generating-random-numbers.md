---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:00.606256-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 것입니다. 프로그래머들은 게임, 시뮬레이션, 보안 알고리즘 등에서 사용하기 위해 랜덤성을 필요로 합니다.

## How to: (방법)
Fish Shell에서 랜덤 숫자를 생성하는 건 간단합니다. `random` 내장 함수를 이용하세요.

```Fish Shell
# 1부터 100 사이의 랜덤 숫자 생성
set random_number (random 1 100)
echo $random_number
```

출력 예시:

```Shell
42
```

여러 개의 숫자가 필요하다면, `-n` 옵션을 사용하세요.

```Fish Shell
# 1부터 100 사이에서 5개의 랜덤 숫자 생성
for i in (seq 5)
    echo (random 1 100)
end
```

출력 예시:

```Shell
58
73
22
99
17
```

## Deep Dive (심층 분석)
Fish Shell은 `random` 함수를 사용하여 랜덤 숫자를 생성합니다. 이전 버전의 Fish에서는 `random` 함수가 없어 `shuf` 또는 `awk` 같은 외부 도구를 사용했었습니다. `random` 함수는 더 간결하며, Fish Shell에 내장되어 있어서 추가 설치없이 사용할 수 있는 장점이 있죠.

다른 프로그래밍 언어나 쉘에서는 랜덤 숫자 생성 방법이 다를 수 있습니다. 예를 들어, Bash에서는 `$RANDOM` 변수를 사용하고, Python에서는 `random` 모듈을 사용합니다.

Fish의 `random`은 난수 생성기(Pseudo-Random Number Generator, PRNG)를 기반으로 하며, 이는 완벽한 랜덤을 보장하지는 않습니다. 하지만 대부분의 응용 프로그램에는 충분합니다. 만약 보안 관련 작업에 사용해야 한다면, 더욱 강력한 암호화 난수 생성기(Cryptographically Secure Pseudo-Random Number Generator, CSPRNG)를 고려해야 합니다.

## See Also (함께 보기)
- Fish Shell 공식 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- 깃허브 Fish Shell 저장소: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- `shuf` 명령어 정보: [https://linux.die.net/man/1/shuf](https://linux.die.net/man/1/shuf)
- `awk` 프로그래밍 언어: [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html)
