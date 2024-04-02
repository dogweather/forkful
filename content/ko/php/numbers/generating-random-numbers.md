---
date: 2024-01-27 20:34:56.292817-07:00
description: "PHP\uC5D0\uC11C \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\
  \uB294 \uAC83\uC740 \uC9C0\uC815\uB41C \uBC94\uC704 \uB0B4\uC5D0\uC11C \uC608\uCE21\
  \uD560 \uC218 \uC5C6\uB294 \uAC12\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC5D0 \uAD00\
  \uD55C \uAC83\uC73C\uB85C, \uC774\uB294 \uACE0\uC720\uD55C \uC0AC\uC6A9\uC790 ID\
  \ \uC0DD\uC131, \uBE44\uBC00\uBC88\uD638 \uC0DD\uC131, \uC2DC\uBBAC\uB808\uC774\uC158\
  \ \uBC0F \uAC8C\uC784 \uC0AC\uC6A9 \uB4F1\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0\
  \ \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uD14C\
  \uC2A4\uD305\uC774\uB098 \uC0AC\uC6A9\uC790 \uACBD\uD5D8\uC744 \uB354\uC6B1 \uACAC\
  \uACE0\uD558\uACE0 \uB9E4\uB825\uC801\uC73C\uB85C\u2026"
lastmod: '2024-03-13T22:44:55.351145-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\
  \uB294 \uAC83\uC740 \uC9C0\uC815\uB41C \uBC94\uC704 \uB0B4\uC5D0\uC11C \uC608\uCE21\
  \uD560 \uC218 \uC5C6\uB294 \uAC12\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC5D0 \uAD00\
  \uD55C \uAC83\uC73C\uB85C, \uC774\uB294 \uACE0\uC720\uD55C \uC0AC\uC6A9\uC790 ID\
  \ \uC0DD\uC131, \uBE44\uBC00\uBC88\uD638 \uC0DD\uC131, \uC2DC\uBBAC\uB808\uC774\uC158\
  \ \uBC0F \uAC8C\uC784 \uC0AC\uC6A9 \uB4F1\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0\
  \ \uD544\uC218\uC801\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uD14C\
  \uC2A4\uD305\uC774\uB098 \uC0AC\uC6A9\uC790 \uACBD\uD5D8\uC744 \uB354\uC6B1 \uACAC\
  \uACE0\uD558\uACE0 \uB9E4\uB825\uC801\uC73C\uB85C\u2026"
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 무엇 & 왜?

PHP에서 무작위 숫자를 생성하는 것은 지정된 범위 내에서 예측할 수 없는 값을 생성하는 것에 관한 것으로, 이는 고유한 사용자 ID 생성, 비밀번호 생성, 시뮬레이션 및 게임 사용 등과 같은 작업에 필수적입니다. 프로그래머는 테스팅이나 사용자 경험을 더욱 견고하고 매력적으로 만들기 위해 애플리케이션에 예측 불가능성과 변화를 더하는 데 무작위성에 의존합니다.

## 어떻게:

PHP는 무작위 숫자를 생성하기 위한 여러 함수를 제공하지만, 가장 일반적으로 사용되는 것은 `rand()`, `mt_rand()`, 그리고 암호화 목적을 위한 `random_int()`입니다.

`rand()`에 의해 반환되는 가장 큰 가능한 값인 getrandmax()와 0 사이의 간단한 무작위 숫자를 생성하려면 다음을 사용할 수 있습니다:

```PHP
echo rand();
```

예를 들어 1부터 100 사이와 같은 더 구체적인 범위의 경우:

```PHP
echo rand(1, 100);
```

그러나 속도와 무작위성 측면에서 `mt_rand()`가 더 나은 선택입니다:

```PHP
echo mt_rand(1, 100);
```

출력은 무작위화에 따라 1부터 100 사이의 어떤 것이 될 수 있으며, 예를 들어 `42`일 수 있습니다.

암호화나 보안 문맥에서 예측할 수 없음이 중요한 경우, 시스템의 무작위 수 생성기에서 무작위 바이트를 사용하여 암호학적으로 안전한 의사-무작위 정수를 생성하는 `random_int()`가 선호되는 선택입니다:

```PHP
echo random_int(1, 100);
```

다시 말하지만 출력은 `84`와 같이 1부터 100 사이의 무작위 숫자이지만, 무작위성의 강력한 보장이 있습니다.

## 자세히 살펴보기

`rand()` 함수는 PHP 초기 버전부터 존재해 왔으며, 무작위 숫자를 생성하기 위한 초기 접근 방식으로 사용되었습니다. 그러나 상대적으로 예측 가능한 알고리즘 때문에 높은 수준의 무작위성을 요구하는 응용 프로그램에는 최선의 선택이 아닙니다.

PHP 4에 도입된 `mt_rand()`는 메르센 트위스터 알고리즘을 기반으로 하며, `rand()`에 비해 속도와 생성할 수 있는 무작위성 측면에서 훨씬 우수합니다. 빠르게 대부분의 비암호화 요구 사항에 대한 선호되는 옵션이 되었습니다.

보안에 민감한 응용 프로그램의 경우, PHP 7에서 시스템의 무작위 수 생성기에서 무작위 바이트를 사용하여 암호학적으로 안전한 의사-무작위 정수를 생성하는 `random_int()`가 도입되었습니다. `rand()`나 `mt_rand()`보다 훨씬 더 안전하며, 예측 가능성이 보안 취약점으로 이어질 수 있는 토큰, 키 또는 다른 요소를 생성하는 데 최선의 선택입니다.

이러한 개선에도 불구하고, 애플리케이션의 맥락에 맞는 올바른 기능을 선택하는 것이 중요합니다. 일반적인 사용을 위해서는 `mt_rand()`가 충분하지만, 목표로 삼거나 악용될 수 있는 것에 대해서는 보안과 무작위성을 모두 제공하는 `random_int()`이 가는 길입니다.
