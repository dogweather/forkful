---
title:                "디버거 사용하기"
date:                  2024-01-26T03:48:28.819744-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용한다는 것은 코드를 면밀히 조사하기 위해 돋보기를 장착하는 것과 같습니다. 프로그래머들은 버그를 제거하고, 흐름을 이해하며, 자신들의 논리가 예상대로 펼쳐지는지 확인하기 위해 이렇게 합니다.

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