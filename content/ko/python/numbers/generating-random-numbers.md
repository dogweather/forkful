---
date: 2024-01-27 20:35:19.885623-07:00
description: "\uC784\uC758\uC758 \uC22B\uC790\uB97C \uC0DD\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC6B0\uC5F0\uBCF4\uB2E4 \uB354 \uB098\uC740 \uC608\uCE21\uC744 \uD560\
  \ \uC218 \uC5C6\uB294 \uC22B\uC790\uB97C \uB9CC\uB4DC\uB294 \uAC83\uC744 \uB9D0\uD558\
  \uBA70, \uC774\uB294 \uC2DC\uBBAC\uB808\uC774\uC158, \uAC8C\uC784, \uBCF4\uC548\
  \ \uC54C\uACE0\uB9AC\uC998 \uAC1C\uBC1C\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uBD88\uD655\
  \uC2E4\uC131\uC744 \uB3C4\uC785\uD558\uAC70\uB098 \uC2E4\uC81C \uC138\uACC4 \uD604\
  \uC0C1\uC744 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC2DC\uBBAC\uB808\
  \uC774\uC158\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.533078
model: gpt-4-0125-preview
summary: "\uC784\uC758\uC758 \uC22B\uC790\uB97C \uC0DD\uC131\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uC6B0\uC5F0\uBCF4\uB2E4 \uB354 \uB098\uC740 \uC608\uCE21\uC744 \uD560 \uC218\
  \ \uC5C6\uB294 \uC22B\uC790\uB97C \uB9CC\uB4DC\uB294 \uAC83\uC744 \uB9D0\uD558\uBA70\
  , \uC774\uB294 \uC2DC\uBBAC\uB808\uC774\uC158, \uAC8C\uC784, \uBCF4\uC548 \uC54C\
  \uACE0\uB9AC\uC998 \uAC1C\uBC1C\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uBD88\uD655\uC2E4\
  \uC131\uC744 \uB3C4\uC785\uD558\uAC70\uB098 \uC2E4\uC81C \uC138\uACC4 \uD604\uC0C1\
  \uC744 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC2DC\uBBAC\uB808\uC774\
  \uC158\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임의의 숫자를 생성한다는 것은 우연보다 더 나은 예측을 할 수 없는 숫자를 만드는 것을 말하며, 이는 시뮬레이션, 게임, 보안 알고리즘 개발에 필수적입니다. 프로그래머들은 이를 통해 불확실성을 도입하거나 실제 세계 현상을 애플리케이션에서 시뮬레이션하기 위해 사용합니다.

## 방법:

Python은 다양한 용도로 임의의 숫자를 생성하는 데 도움이 되는 `random` 모듈을 제공합니다. 시작하는 방법은 다음과 같습니다:

1. **모듈 가져오기**
    ```Python
    import random
    ```

2. **임의의 정수 생성**
    두 숫자 사이에서.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    샘플 출력: `7`

3. **부동 소수점 생성**
    0과 1 사이에서.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    샘플 출력: `0.436432634653`

    다른 범위의 부동 소수점이 필요한 경우 곱하십시오:
    ```Python
    random_float_range = random.random() * 5  # 0에서 5까지
    print(random_float_range)
    ```
    샘플 출력: `3.182093745`

4. **리스트에서 임의의 요소 선택**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    샘플 출력: `Hola`

5. **리스트 섞기**
    카드 게임이나 순서를 무작위로 해야 하는 모든 애플리케이션에 적합합니다.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    샘플 출력: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## 심화 탐구

Python의 `random` 모듈은 의사 난수 생성기(PRNG)인, 특히 일반적인 용도에 좋은 메르센 트위스터 알고리즘을 사용합니다. 그러나 충분한 출력이 관찰되면 예측 가능성 때문에 암호학적 용도에는 적합하지 않습니다. Python 3.6에서 도입된 `secrets` 모듈은 암호학적으로 강력한 임의의 숫자를 생성하는 데 더 나은 대안을 제공하며, 특히 보안에 민감한 애플리케이션에서 유용합니다. 예를 들어, 비밀번호 재설정 링크에 대한 안전하고 임의의 토큰을 생성하는 경우:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

역사적으로 진정으로 임의의 숫자를 생성하는 것은 컴퓨팅에서 도전이었으며, 초기 방법은 물리 현상이나 수동으로 입력된 시드에 의존했습니다. 메르센 트위스터(적어도 2023년까지 마지막 지식 업데이트 이전까지 Python의 `random` 모듈에서 기본적으로 사용된)와 같은 알고리즘의 개발 및 채택은 중요한 진전을 표시했습니다. 그러나 보안과 효율성에 대한 더 많은 요구는 암호학 관련 작업을 위한 `secrets` 모듈의 포함으로 이어졌으며, 이러한 진화는 소프트웨어 개발에서 보안의 중요성이 커지고 암호화부터 안전한 토큰 생성에 이르기까지 다양한 애플리케이션에서 더 강력한 무작위성이 필요하다는 요구를 반영합니다.
