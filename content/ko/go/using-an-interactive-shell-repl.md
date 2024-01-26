---
title:                "인터랙티브 셸 (REPL) 사용하기"
date:                  2024-01-26T04:14:43.620448-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?
REPL(Read-Eval-Print Loop, 읽기-평가-출력 루프)는 코드와 실시간으로 상호 작용할 수 있게 해주며, 입력을 읽고, 평가한 후 결과를 출력하고 다시 시작하는 과정을 반복합니다. 프로그래머는 이를 사용하여 코드 조각을 테스트하고, 디버그하며, 실시간으로 새로운 언어를 학습합니다.

## 사용 방법:
Go는 내장 REPL을 포함하고 있지 않지만, 타사 도구를 사용할 수 있습니다. 인기 있는 도구 중 하나는 `gore`입니다:

```go
// gore 설치하기
$ go install github.com/motemen/gore/cmd/gore@latest

// gore 실행하기
$ gore
gore 버전 0.5.0  :도움말을 위한 :help
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## 심화 학습
원래 Lisp용으로 개발된 REPL은 Python이나 Ruby와 같은 동적 언어에서 일반적입니다. Go는 정적 타입 언어이기 때문에 기본적으로 제공되지 않습니다. `gore`에 대한 대안으로는 `go-pry`와 `yaegi`가 있습니다. 이 도구들은 Go 코드를 해석하여, 전체 앱을 컴파일하지 않고도 빠르게 아이디어를 탐색하고 검증할 수 있게 합니다. 특히 초보자나 학습과 실험에 중점을 둔 교육적 상황에서 유용합니다.

## 참고 자료
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)