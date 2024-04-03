---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:28.085096-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Go\uB294 \uB0B4\uC7A5\uB41C REPL\uC744 \uD3EC\
  \uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC9C0\uB9CC, \uCEE4\uBBA4\uB2C8\uD2F0\uB294\
  \ `gore`\uC640 \uAC19\uC740 \uB3C4\uAD6C\uB97C \uB9CC\uB4E4\uC5B4 \uC774 \uACF5\uBC31\
  \uC744 \uCC44\uC6E0\uC2B5\uB2C8\uB2E4. \uBA3C\uC800, \uB2E4\uC74C\uC744 \uC2E4\uD589\
  \uD558\uC5EC `gore`\uB97C \uC124\uCE58\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.458555-06:00'
model: gpt-4-0125-preview
summary: "Go\uB294 \uB0B4\uC7A5\uB41C REPL\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0\
  \ \uC54A\uC9C0\uB9CC, \uCEE4\uBBA4\uB2C8\uD2F0\uB294 `gore`\uC640 \uAC19\uC740 \uB3C4\
  \uAD6C\uB97C \uB9CC\uB4E4\uC5B4 \uC774 \uACF5\uBC31\uC744 \uCC44\uC6E0\uC2B5\uB2C8\
  \uB2E4."
title: "\uB300\uD654\uD615 \uC258(REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

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
