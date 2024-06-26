---
date: 2024-01-27 20:33:35.135519-07:00
description: "\uC5B4\uB5BB\uAC8C: Fish\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\
  \uD558\uB294 \uAC83\uC740 \uC2DC\uC2A4\uD15C \uC720\uD2F8\uB9AC\uD2F0\uC640 \uC258\
  \ \uB2A5\uB825\uC758 \uC870\uD569\uC744 \uC0AC\uC6A9\uD558\uC5EC \uAC04\uB2E8\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uC9C0\uC815\uB41C \uBC94\uC704\
  \ \uB0B4\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uBC29\uBC95\uC744\
  \ \uBCF4\uC5EC\uC8FC\uB294 \uBA87 \uAC00\uC9C0 \uC608\uC785\uB2C8\uB2E4. **0\uBD80\
  \uD130 100 \uC0AC\uC774\uC758 \uB09C\uC218 \uC0DD\uC131:**."
lastmod: '2024-03-13T22:44:55.846556-06:00'
model: gpt-4-0125-preview
summary: "Fish\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uC740\
  \ \uC2DC\uC2A4\uD15C \uC720\uD2F8\uB9AC\uD2F0\uC640 \uC258 \uB2A5\uB825\uC758 \uC870\
  \uD569\uC744 \uC0AC\uC6A9\uD558\uC5EC \uAC04\uB2E8\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 어떻게:
Fish에서 난수를 생성하는 것은 시스템 유틸리티와 쉘 능력의 조합을 사용하여 간단할 수 있습니다. 아래는 지정된 범위 내에서 난수를 생성하는 방법을 보여주는 몇 가지 예입니다.

**0부터 100 사이의 난수 생성:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**샘플 출력:**
```fish
42
```

**임의의 두 숫자 사이에서 난수 생성하기, 예를 들어 50과 150 사이:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**샘플 출력:**
```fish
103
```

**리스트를 무작위로 섞기 위해 random 사용하기:**

리스트의 요소를 무작위로 섞고 싶을 수도 있습니다. 이렇게 하는 방법은 다음과 같습니다:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**샘플 출력:**
```fish
C
A
E
D
B
```

이 명령들을 실행할 때마다 출력은 난수의 성격으로 인해 매번 달라질 것임을 주의하세요.

## 심화 탐구
Fish Shell의 `random` 함수는 난수를 만들어내기 위한 사용하기 쉬운 인터페이스를 제공합니다. 내부적으로, 시스템 수준의 난수 생성 유틸리티를 둘러싼 것으로, 스크립트에 난수를 도입하는 휴대성 있는 방법을 제공합니다. 그러나, `random`에 의해 제공되는 난수는 대부분의 스크립팅 작업에 충분하지만, 높은 수준의 예측 불가능성이 필요한 애플리케이션을 위한 암호화 보안 요구 사항을 충족시키지 못할 수 있습니다.

높은 보안 맥락에서, 암호화 목적을 위해 설계된 전용 도구나 프로그래밍 라이브러리를 사용하는 것을 고려해 보십시오. 이들은 더 강력한 난수 보장을 제공합니다. 그럼에도 불구하고, 일반 스크립팅과 난수에 대한 최고 수준의 보안 표준이 요구되지 않는 애플리케이션에 대해서는, Fish Shell의 `random` 함수가 편리하고 효과적인 해결책을 제공합니다.
