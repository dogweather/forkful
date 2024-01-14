---
title:    "Clojure: 새로운 프로젝트 시작하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

Korean Translation:

## 왜

새로운 프로젝트를 시작하는 이유는 무엇일까요? Clojure 프로그래밍을 배우고 싶거나 더 나은 프로젝트를 만들고 싶은 욕망이 있기 때문입니다.

## 시작하는 법

Clojure 언어를 사용하여 새로운 프로젝트를 시작하는 방법을 살펴보겠습니다. 우리는 `def` 함수와 `println` 함수를 사용하여 간단한 "Hello, world!" 프로그램을 만들어 볼 것입니다.

먼저, Clojure REPL(Read-Eval-Print Loop)을 열어보세요. REPL은 Clojure의 대화형 쉘(Command-Line Interface)로, 코드를 입력하고 실행 결과를 즉시 확인할 수 있습니다.

```Clojure
(def hello "Hello, world!")
(println hello)
```

위의 코드를 REPL에 입력하면 `Hello, world!`라는 출력을 볼 수 있을 것입니다.

여러분은 이제 Clojure 프로그래밍을 시작할 준비가 되었습니다!

## 깊이 파고들기

새로운 프로젝트를 시작하면서 더욱 깊이 있는 정보를 알고 싶을 수 있습니다. Clojure는 함수형 프로그래밍 언어로, 함수를 이용하여 데이터를 처리하는 방식을 중요하게 생각합니다. 또한, Clojure의 데이터 구조는 변경 불가능(immutable)하고, 스레드 안전(thread-safe)하며, 기본적인 연산이 빠른 특징을 가지고 있습니다.

Clojure에서는 "코드는 데이터다"라는 철학을 따르고 있습니다. 따라서 코드는 Clojure 자체의 데이터 구조로 표현되며, 이는 매크로(macro)를 통해 코드를 조작하는 동적 프로그래밍(dynamic programming)이 가능하게 합니다.

더욱 자세한 내용은 [Clojure 공식 문서](https://clojure.org/)와 [Clojure 공식 튜토리얼](https://clojure.org/guides/getting_started)을 참고해보세요. Clojure 언어를 더욱 쉽게 익히기 위한 [ClojureBridge](http://clojurebridge.org/)와 같은 온라인 커뮤니티도 존재합니다.

## 다른 글들 참고하기

이 글에서는 Clojure 언어를 소개하고, 새로운 프로젝트를 시작하는 방법을 알아보았습니다. 하지만 Clojure에는 더 많은 기능과 개념이 존재합니다. 다른 관련 글들을 읽어보며 더욱 전문적으로 Clojure 프로그래밍을 할 수 있도록 노력해보세요.

- [Clojure의 함수형 프로그래밍 개념](https://clojure.org/guides/learn/functions)
- [Clojure의 데이터 구조](https://clojure.org/reference/data_structures)
- [Clojure의 매크로 개념](https://clojure.org/guides/weird_characters#_macros)
- [ClojureBridge의 온라인 커뮤니티](http://clojurebridge.org/community/)