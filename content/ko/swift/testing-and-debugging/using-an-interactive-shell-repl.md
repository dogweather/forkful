---
date: 2024-01-26 04:18:16.079240-07:00
description: "\uB300\uD654\uD615 \uC178, \uB610\uB294 Read-Eval-Print Loop(REPL)\uC744\
  \ \uC0AC\uC6A9\uD558\uBA74 \uC0C1\uD638 \uC791\uC6A9\uC801\uC73C\uB85C \uCF54\uB4DC\
  \uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC Swift \uCF54\uB4DC \uC870\
  \uAC01\uC744 \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\uD558\uAC70\uB098, \uB514\uBC84\
  \uAE45\uD558\uAC70\uB098, \uC5B8\uC5B4\uB97C \uD559\uC2B5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.734054-06:00'
model: gpt-4-0125-preview
summary: "\uB300\uD654\uD615 \uC178, \uB610\uB294 Read-Eval-Print Loop(REPL)\uC744\
  \ \uC0AC\uC6A9\uD558\uBA74 \uC0C1\uD638 \uC791\uC6A9\uC801\uC73C\uB85C \uCF54\uB4DC\
  \uB97C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC Swift \uCF54\uB4DC \uC870\
  \uAC01\uC744 \uBE60\uB974\uAC8C \uD14C\uC2A4\uD2B8\uD558\uAC70\uB098, \uB514\uBC84\
  \uAE45\uD558\uAC70\uB098, \uC5B8\uC5B4\uB97C \uD559\uC2B5\uD569\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 무엇인가 & 왜 사용하는가?
대화형 셸, 또는 Read-Eval-Print Loop(REPL)을 사용하면 상호 작용적으로 코드를 작성할 수 있습니다. 프로그래머들은 이를 사용하여 Swift 코드 조각을 빠르게 테스트하거나, 디버깅하거나, 언어를 학습합니다.

## 사용 방법:
터미널을 열고 `swift`를 실행하여 REPL을 호출합니다. 코드를 직접 입력하고 Enter 키를 눌러 실행합니다. 맛보기 예시입니다:

```Swift
1> let greeting = "안녕, REPL!"
greeting: String = "안녕, REPL!"
2> print(greeting)
안녕, REPL!
```

`:quit`나 `Control-D`를 사용하여 종료합니다.

## 심층 탐구
REPL의 뿌리는 60년대 리스프 해석기까지 거슬러 올라갑니다. Swift의 REPL은 LLVM이라는 강력한 컴파일러 프레임워크 위에 구축되어 있으며, 기본 해석 기능 이상을 제공합니다. 자동완성, 디버깅 등이 가능한 완전한 도구입니다. REPL은 학습이나 프로토타이핑에는 훌륭하지만, 독립적인 개발 환경은 아닙니다. 일부 사람들은 더 그래픽적이고 파일 기반의 접근 방식인 Xcode의 Playgrounds를 선호하는 반면, 다른 이들은 전통적인 스크립트 편집 및 실행을 고수합니다.

내부적으로, Swift의 REPL은 코드를 동적으로 기계어로 컴파일하고 실행합니다. 이 때문에 상대적으로 빠릅니다. 또한 컴파일된 Swift 모듈이나 심지어 C 라이브러리에도 접근할 수 있어 매우 강력합니다. 하지만 REPL에서 완벽하게 작동하지 않는 것들이 있습니다; 복잡한 프로젝트 설정이나 스토리보드 파일을 필요로 하는 일부 Swift 기능은 여기서 사용할 수 없습니다.

## 참고 자료
- [Swift.org - 시작하기](https://www.swift.org/getting-started/#using-the-repl)
- 애플의 [Xcode Playgrounds 소개](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM 프로젝트](https://llvm.org/)
