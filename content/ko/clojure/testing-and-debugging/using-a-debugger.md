---
date: 2024-01-26 03:48:28.819744-07:00
description: "\uBC29\uBC95: \uD074\uB85C\uC800\uB294 \uC790\uBC14 \uAC00\uC0C1 \uBA38\
  \uC2E0(JVM)\uC5D0 \uC758\uC874\uD558\uBBC0\uB85C \uB9CE\uC740 \uB514\uBC84\uAE45\
  \uC774 \uC790\uBC14 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774\uB8E8\uC5B4\
  \uC9D1\uB2C8\uB2E4. \uADF8\uC911 \uD558\uB098\uC778 `CIDER`\uB294 Emacs\uC5D0\uC11C\
  \ \uD074\uB85C\uC800 \uAC1C\uBC1C\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uD328\uD0A4\
  \uC9C0\uB85C, \uD0C4\uD0C4\uD55C \uB514\uBC84\uAE45 \uAE30\uB2A5\uC744 \uAC16\uCD94\
  \uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uC790\uC138\uD788 \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.667469-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uB294 \uC790\uBC14 \uAC00\uC0C1 \uBA38\uC2E0(JVM)\uC5D0\
  \ \uC758\uC874\uD558\uBBC0\uB85C \uB9CE\uC740 \uB514\uBC84\uAE45\uC774 \uC790\uBC14\
  \ \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774\uB8E8\uC5B4\uC9D1\uB2C8\uB2E4\
  ."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 방법:
클로저는 자바 가상 머신(JVM)에 의존하므로 많은 디버깅이 자바 도구를 사용하여 이루어집니다. 그중 하나인 `CIDER`는 Emacs에서 클로저 개발을 위한 강력한 패키지로, 탄탄한 디버깅 기능을 갖추고 있습니다. 자세히 살펴보겠습니다:

```clojure
;; 먼저, Emacs 내에서 CIDER를 사용해 클로저 프로젝트에 연결합니다.
M-x cider-jack-in

;; 중단점 설정
;; 검사하고 싶은 클로저 코드의 줄로 이동하고
;; "C-c M-b"를 누르거나 다음을 실행합니다:
M-x cider-debug-defun-at-point

;; 코드를 실행하면, 중단점에 도달합니다. CIDER는 다음과 같이 프롬프트를 표시합니다:
;; 1. n을 눌러 실행의 다음 논리적 단계로 이동,
;; 2. c를 눌러 다음 중단점까지 실행을 계속,
;; 3. q를 눌러 디버깅을 종료.

;; 중단점에서 지역 변수 검사
;; 중단점에 있을 때 다음을 입력합니다:
locals

;; minibuffer에 지역 변수와 그 값의 목록이 출력됩니다.
```
샘플 출력은 다음과 같을 수 있습니다:
```clojure
{:x 10, :y 20, :result 200}
```

## 심층 분석
디버거는 컴퓨팅 용어로 오래된 도구입니다. "버그"라는 용어는 컴퓨팅 초기, 실제 곤충이 기계 내 회로를 단락시켜 오류를 발생시킨 사건에서 유래되었습니다.

`CIDER`는 Emacs 애호가들에게 훌륭하지만, 클로저 디버깅을 위한 대안들도 있습니다. 예를 들어, IntelliJ와 Cursive 플러그인을 사용하면 더 GUI 기반의 디버깅 경험을 얻을 수 있습니다. 게다가, 디버깅 시 프로세스 흐름을 제어하기 위해 내장된 Leiningen이나 tools.deps을 사용할 수 있습니다.

내부적으로, 이러한 디버거들은 종종 바이트코드를 조작하고, 전용 nREPL 세션에서 평가를 수행하며, 스택 트레이스 검사를 제공합니다. 그들은 기본적인 JVM의 능력을 활용하며, 자바의 디버깅 프레임워크의 풍부함에 접근합니다.

## 참고
- [CIDER 디버거 문서](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive 디버거](https://cursive-ide.com/userguide/debugging.html)
- [자동화 및 디버깅을 위한 Leiningen](https://leiningen.org/)
- [더 많은 제어를 위한 tools.deps.alpha](https://github.com/clojure/tools.deps.alpha)
