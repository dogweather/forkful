---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 해야하나요?

새로운 프로젝트를 시작하는 것은 무엇인가요? 이것은 새로운 아이디어와 목표를 구현하기 위해 코드를 작성하는 첫 단계입니다. 프로그래머들은 왜 이를 수행하나요? 이는 새로운 솔루션을 제공하거나 기존 문제를 해결하기 위해서입니다.

## 어떻게 하나요:
Gleam 언어로 새 프로젝트를 시작하는 방법을 산출해 봅시다.
```Gleam
gleam new hello_gleam
cd hello_gleam
gleam run
```
위를 실행하면 다음과 같은 출력이 나옵니다:
```Gleam
Hello, from Gleam!
```
이처럼 간단! 이제부터 우리의 모험이 시작된다는 것을 확인했죠?

## 깊게 이해하기:
새로운 프로젝트를 시작함으로써, 개발의 역사적 측면에 대해 알아볼 수 있습니다. 프로그래밍을 하면서 우리는 이전에 작성된 코드나 다른 프로젝트에서 착안한 아이디어를 사용해서, 기존 문제를 해결하거나 새로운 솔루션을 제시하는 것이 일반적입니다.

대안으로는 여러 언어를 사용할 수 있으며, 각 언어는 특정 문제를 해결하는데 초점을 맞추고 있습니다. 예를 들어, Python은 일반적인 목적으로 널리 사용되며, JavaScript는 웹 개발에 유용하고, C는 시스템 프로그래밍에 유용합니다.

Gleam 프로젝트의 생성은 패키징 및 의존성 관리를 위한 rebar3, 시스템 및 도구 통합을 위한 escript를 사용하여 이뤄집니다. 이런 메커니즘으로 인해 개발자는 복잡한 작업을 쉽게 수행할 수 있습니다.

## 참고 자료:
1. Gleam 언어 공식 웹사이트: https://gleam.run/
2. rebar3 공식 문서: https://www.rebar3.org/
3. escript 공식 문서: http://erlang.org/doc/man/escript.html
이 세 가지는 Gleam 개발에 유용한 참고 자료가 될 수 있습니다. 이런 리소스를 통해서 더 깊고 넓게 이해하고 배울 수 있습니다.