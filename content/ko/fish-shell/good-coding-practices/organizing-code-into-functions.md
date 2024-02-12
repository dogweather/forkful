---
title:                "코드를 함수로 구성하는 방법"
aliases:
- /ko/fish-shell/organizing-code-into-functions/
date:                  2024-01-28T23:01:54.005907-07:00
model:                 gpt-4-0125-preview
simple_title:         "코드를 함수로 구성하는 방법"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
코드를 함수로 구성한다는 것은 특정 작업을 수행하기 위해 스크립트의 일부를 묶는 것에 대해 이야기합니다. 코드를 더 쉽게 읽고, 테스트하며, 재사용할 수 있게 하기 위해서입니다 — 아무도 코드 스파게티의 늪을 헤치고 싶어하지 않습니다.

## 방법:
Fish에서 함수를 작성하는 방법은 `function` 키워드를 사용하고 이름을 붙인 다음, `end`로 끝냅니다. 여기 간단한 예가 있습니다:

```fish
function hello
    echo "Hello, World!"
end

hello
```

출력:
```
Hello, World!
```

이제 사용자에게 인사하는 함수를 만들어 보겠습니다:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

출력:
```
Hey there, your_username!
```

세션간에 유지하려면 `funcsave greet`을 사용하세요.

## 심층 탐구
Fish Shell 함수들은 미니 스크립트와 같습니다 — 거의 모든 것을 넣을 수 있습니다. 역사적으로, 쉘 스크립팅에서 함수의 개념은 반복적인 타이핑과 디버깅에 들어가는 수많은 시간을 절약했습니다. Python과 같은 프로그래밍 언어와 달리, Shell 함수는 구조보다는 편의성에 관한 것입니다.

Bash와 같은 몇몇 쉘은 `function`이나 그냥 중괄호를 사용합니다. Fish는 `function ... end`— 명확하고 읽기 쉽습니다. Fish 함수 내부에서는 매개변수, `set -l`을 사용한 로컬 변수, 심지어 다른 함수 내에 함수를 정의할 수도 있는 모든 기능을 사용할 수 있습니다.

Fish는 그다지 중요시하지 않기 때문에 `return` 값이 필요 없습니다; 함수의 출력이 그 반환값입니다. 그리고 향후 세션에도 지속적으로 사용할 수 있는 함수를 원한다면 `funcsave`를 기억하세요.

## 참고

- 함수에 관한 fish 튜토리얼: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### 함수 명령어

- [function](https://fishshell.com/docs/current/cmds/function.html) — 함수 생성
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — 함수를 출력하거나 지움
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — 사용자의 자동 로드 디렉토리에 함수의 정의를 저장
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — 함수를 상호 작용적으로 편집
