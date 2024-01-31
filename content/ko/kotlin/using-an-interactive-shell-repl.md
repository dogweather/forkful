---
title:                "인터랙티브 셸 (REPL) 사용하기"
date:                  2024-01-26T04:15:55.740570-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
REPL(Read-Eval-Print Loop, 읽기-평가-출력 반복)은 단순한 인터랙티브 컴퓨터 프로그래밍 환경입니다. 프로그래머들은 이를 빠른 코딩 시도, 코드 조각 테스트, 전체 애플리케이션을 만들지 않고 언어의 문법을 학습하기 위해 사용합니다.

## 방법:
Kotlin의 REPL을 시작하는 것은 매우 간단합니다. 터미널을 열고 `kotlinc`라고 입력하세요. 그러면 Kotlin 셸에 들어갑니다. 변수를 정의하고 그 값을 출력해 보겠습니다:

```kotlin
Kotlin 버전 1.7.10에 오신 것을 환영합니다 (JRE 1.8.0_292-b10)
도움말을 보려면 :help를 입력하세요, 종료하려면 :quit를 입력하세요
>>> val 인사말 = "Hello, Kotlin REPL!"
>>> println(인사말)
Hello, Kotlin REPL!
```

## 심층 탐구
Kotlin의 REPL은 언어와 함께 데뷔하여 실험을 장려하도록 만들어졌습니다. Python의 인터랙티브 셸과 유사하지만 Kotlin의 문법과 특성에 맞게 조정되었습니다. 대안? IntelliJ IDEA와 같은 IDE의 인터랙티브 환경과 온라인 Kotlin 놀이터들입니다. REPL은 코드를 즉석에서 컴파일함으로써 즉각적인 피드백을 제공합니다 – 학습과 디버깅에 필수적입니다.

## 참고하세요
- REPL에 대한 Kotlin 문서: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- 브라우저에서 Kotlin 사용해 보기: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- IntelliJ IDEA용 JetBrains Kotlin Playground 플러그인.
