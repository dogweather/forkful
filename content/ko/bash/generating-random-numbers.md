---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:52.597308-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

랜덤 숫자 생성은 예측 불가능한 숫자를 만드는 것입니다. 프로그래머들은 테스트, 보안, 게임 등 여러 분야에서 예측 할 수 없는 결과를 필요로 할 때 이 기술을 사용합니다.

## How to: (방법)

Bash에서 랜덤하게 숫자를 생성하는 기본적인 방법은 `$RANDOM` 환경 변수를 사용하는 것입니다.

```Bash
echo $RANDOM
```

출력 예시:

```
29321
```

0과 32767 사이의 숫자를 생성합니다. 다른 범위가 필요하다면, 모듈로 연산자(%)를 활용하세요:

```Bash
echo $((RANDOM % 100))  # 0과 99 사이의 숫자
```

출력 예시:

```
47
```

100과 200 사이의 숫자를 원한다면, 이렇게 조정:

```Bash
echo $((RANDOM % 100 + 100))  # 100과 199 사이의 숫자
```

출력 예시:

```
153
```

## Deep Dive (심층 분석)

`$RANDOM`은 Bash 내장 난수 생성기입니다. 1989년 Bash 버전 1.05에서 처음 소개됐죠. 이 변수는 매번 쉘 스크립트가 접근할 때마다 새로운 난수를 생성하여 반환합니다. 하지만, 이 값은 암호학적인 용도로는 충분히 강력하지 않습니다.

설령 Bash에서 난수를 생성하는 더 복잡한 방법이 요구될 때, `/dev/urandom` 또는 `/dev/random`을 사용할 수 있습니다.

```Bash
head -n 1 /dev/urandom | od -An -N2 -i
```

다른 방법으로, `shuf` 커맨드를 사용하는 것도 가능합니다:

```Bash
shuf -i 1-100 -n 1
```

이 커맨드는 1과 100 사이에서 무작위로 하나의 정수를 선택해 출력합니다.

## See Also (추가 정보)

- Bash man 페이지: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- `shuf` 커맨드에 관한 정보: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- `/dev/urandom`과 `/dev/random`에 관한 논의: https://www.2uo.de/myths-about-urandom/

이러한 리소스를 통해 Bash에서 난수를 생성하고 사용하는 방법에 대해 더 심층적으로 이해할 수 있습니다.
