---
date: 2024-01-27 20:33:35.135519-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\
  \uC740 \uB370\uC774\uD130 \uC0D8\uD50C\uB9C1\uBD80\uD130 \uAC8C\uC784 \uAC1C\uBC1C\
  \uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uBAA8\uB4E0 \uAC83\uC5D0 \uC0AC\uC6A9\uB418\
  \uB294 \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. Fish Shell\uC5D0\
  \uC11C\uB294 \uC2DC\uC2A4\uD15C \uB3C4\uAD6C\uC640 \uB0B4\uC7A5 \uD568\uC218\uB97C\
  \ \uC774\uC6A9\uD558\uC5EC \uC774 \uBAA9\uC801\uC744 \uC704\uD55C \uC791\uC5C5\uC744\
  \ \uD560 \uC218 \uC788\uAC8C \uD568\uC73C\uB85C\uC368, \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC774 \uC2A4\uD06C\uB9BD\uD2B8\uC640 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC5D0 \uBB34\uC791\uC704\uC131\uACFC \uBCC0\uB3D9\uC131\uC744\u2026"
lastmod: '2024-03-13T22:44:55.846556-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740\
  \ \uB370\uC774\uD130 \uC0D8\uD50C\uB9C1\uBD80\uD130 \uAC8C\uC784 \uAC1C\uBC1C\uC5D0\
  \ \uC774\uB974\uAE30\uAE4C\uC9C0 \uBAA8\uB4E0 \uAC83\uC5D0 \uC0AC\uC6A9\uB418\uB294\
  \ \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. Fish Shell\uC5D0\uC11C\
  \uB294 \uC2DC\uC2A4\uD15C \uB3C4\uAD6C\uC640 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC774\
  \uC6A9\uD558\uC5EC \uC774 \uBAA9\uC801\uC744 \uC704\uD55C \uC791\uC5C5\uC744 \uD560\
  \ \uC218 \uC788\uAC8C \uD568\uC73C\uB85C\uC368, \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC2A4\uD06C\uB9BD\uD2B8\uC640 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\
  \ \uBB34\uC791\uC704\uC131\uACFC \uBCC0\uB3D9\uC131\uC744\u2026"
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 무엇과 왜?

프로그래밍에서 난수 생성은 데이터 샘플링부터 게임 개발에 이르기까지 모든 것에 사용되는 기본적인 작업입니다. Fish Shell에서는 시스템 도구와 내장 함수를 이용하여 이 목적을 위한 작업을 할 수 있게 함으로써, 프로그래머들이 스크립트와 애플리케이션에 무작위성과 변동성을 효과적으로 통합할 수 있습니다.

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
