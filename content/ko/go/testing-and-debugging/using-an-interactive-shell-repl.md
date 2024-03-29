---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:28.085096-07:00
description: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 \uC77D\uAE30-\uD3C9\
  \uAC00-\uCD9C\uB825 \uB8E8\uD504(REPL)\uB294 Go \uCF54\uB4DC\uB97C \uC2E4\uC2DC\uAC04\
  \uC73C\uB85C \uC2E4\uD5D8\uD558\uBA74\uC11C \uBA85\uB839\uC744 \uC2E4\uD589\uD558\
  \uACE0 \uC989\uC2DC \uD53C\uB4DC\uBC31\uC744 \uC5BB\uC744 \uC218 \uC788\uAC8C \uD569\
  \uB2C8\uB2E4. \uC774 \uC811\uADFC\uBC95\uC740 \uD559\uC2B5, \uB514\uBC84\uAE45,\
  \ \uD504\uB85C\uD1A0\uD0C0\uC774\uD551\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uBA70\
  , \uC804\uD1B5\uC801\uC778 \uD3B8\uC9D1-\uCEF4\uD30C\uC77C-\uC2E4\uD589 \uC8FC\uAE30\
  \uB97C \uC6B0\uD68C\uD568\uC73C\uB85C\uC368 \uAC1C\uBC1C \uD504\uB85C\uC138\uC2A4\
  \uB97C \uB354 \uBE60\uB974\uACE0\u2026"
lastmod: '2024-03-13T22:44:54.458555-06:00'
model: gpt-4-0125-preview
summary: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 \uC77D\uAE30-\uD3C9\uAC00\
  -\uCD9C\uB825 \uB8E8\uD504(REPL)\uB294 Go \uCF54\uB4DC\uB97C \uC2E4\uC2DC\uAC04\uC73C\
  \uB85C \uC2E4\uD5D8\uD558\uBA74\uC11C \uBA85\uB839\uC744 \uC2E4\uD589\uD558\uACE0\
  \ \uC989\uC2DC \uD53C\uB4DC\uBC31\uC744 \uC5BB\uC744 \uC218 \uC788\uAC8C \uD569\uB2C8\
  \uB2E4. \uC774 \uC811\uADFC\uBC95\uC740 \uD559\uC2B5, \uB514\uBC84\uAE45, \uD504\
  \uB85C\uD1A0\uD0C0\uC774\uD551\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uBA70, \uC804\
  \uD1B5\uC801\uC778 \uD3B8\uC9D1-\uCEF4\uD30C\uC77C-\uC2E4\uD589 \uC8FC\uAE30\uB97C\
  \ \uC6B0\uD68C\uD568\uC73C\uB85C\uC368 \uAC1C\uBC1C \uD504\uB85C\uC138\uC2A4\uB97C\
  \ \uB354 \uBE60\uB974\uACE0\u2026"
title: "\uB300\uD654\uD615 \uC258(REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

인터랙티브 셸, 또는 읽기-평가-출력 루프(REPL)는 Go 코드를 실시간으로 실험하면서 명령을 실행하고 즉시 피드백을 얻을 수 있게 합니다. 이 접근법은 학습, 디버깅, 프로토타이핑에 널리 사용되며, 전통적인 편집-컴파일-실행 주기를 우회함으로써 개발 프로세스를 더 빠르고 직관적으로 만듭니다.

## 사용 방법:

Go는 내장된 REPL을 포함하고 있지 않지만, 커뮤니티는 `gore`와 같은 도구를 만들어 이 공백을 채웠습니다. 먼저, 다음을 실행하여 `gore`를 설치하세요:

```
$ go get -u github.com/motemen/gore
```

설치가 완료되면 터미널에서 `gore`를 입력하여 `gore`를 실행하세요:

```
$ gore
```

Go 명령을 받을 준비가 되어 있는 프롬프트가 나타납니다. 간단한 예제를 시도해 봅시다:

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

다음과 같은 출력을 보게 됩니다:

```
Hello, Go REPL!
```

변수와 함수 정의는 예상대로 작동합니다. 함수를 선언할 수 있습니다:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("반지름이 4인 원의 면적:", areaCircle(4))
```

그리고 바로 출력을 얻을 수 있습니다:

```
반지름이 4인 원의 면적: 50.26548245743669
```

## 심층 탐구:

REPL의 개념은 고대의 것으로, 1960년대의 Lisp 기계로 거슬러 올라가면서 상호작용적인 프로그래밍 경험을 제공했습니다. Python이나 JavaScript와 같은 언어와 달리, Go는 REPL 없이 설계되었으며, 성능과 단순성을 위한 컴파일된 바이너리에 중점을 둡니다. 이는 Go의 단순함 철학과 확장 가능하며 유지보수 가능한 소프트웨어 설계를 반영합니다.

그러나 `gore`나 `goplay`와 같은 도구는 Go 코드를 동적으로 파싱하고 `go/eval` 패키지 또는 비슷한 메커니즘을 사용해 실시간으로 실행함으로써, 이 공백을 메우는 Go 커뮤니티의 자원성을 보여줍니다. 그러나 이러한 도구는, 네이티브 REPL 환경에 비해 몇 가지 제한이 있습니다. 이러한 제한은 Go의 타입 시스템과 컴파일 모델에서 비롯되며, 즉석에서의 평가를 어렵게 만듭니다.

REPL 환경은 교육과 빠른 테스트를 위해 매우 유용하지만, Go 생태계는 대부분의 개발 작업에 대해 전통적인 컴파일 및 실행 프로세스를 선호합니다. Visual Studio Code나 GoLand와 같이 Go 지원을 제공하는 IDE와 편집기는 REPL이 필요하지 않을 정도로 테스트 및 디버깅을 위한 통합 도구를 제공합니다.

그러나 탐구적 프로그래밍, 프로토타이핑, 학습을 위해서는 `gore`와 같은 REPL이 다른 언어에서 REPL에 익숙한 프로그래머들에게 Go에서 유사한 경험을 제공하면서 소중한 대안이 됩니다.
