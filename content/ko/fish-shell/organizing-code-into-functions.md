---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:10:30.170666-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 함수를 사용하여 코드 정리하기: Fish Shell

### 무엇이며 왜 사용하는가?
코드를 함수로 구성하는 것은 특정 작업을 수행하기 위해 스크립트의 일부를 묶는 것을 말합니다. 이를 통해 코드를 읽고, 테스트하고, 재사용하기가 더 쉬워집니다. 아무도 코드 스파게티의 늪을 헤치고 싶어 하지 않습니다.

### 사용법:
Fish에서는 `function` 키워드로 함수를 작성하고 이름을 지정한 다음 `end`로 끝맺습니다. 여기 간단한 예가 있습니다:

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

이제 사용자에게 인사하는 함수를 만들어 봅시다:

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

세션 간에 함수를 저장하고 싶다면 `funcsave greet`를 사용하세요.

### 심층 분석
Fish Shell 함수는 미니 스크립트와 같습니다 — 거의 모든 것을 넣을 수 있습니다. 역사적으로, 셸 스크립팅에서의 함수 개념은 반복적인 타이핑과 디버깅에서 수많은 시간을 절약해 주었습니다. Python과 같은 프로그래밍 언어와 달리, Shell 함수는 구조보다는 편의성에 관한 것입니다.

Bash와 같은 일부 셸은 `function`이나 그냥 중괄호를 사용합니다. Fish는 `function ... end`를 사용해 — 명확하고 읽기 쉽습니다. Fish 함수 안에서는 모든 장점을 다 사용할 수 있습니다: 매개변수, `set -l`을 이용한 로컬 변수, 심지어 한 함수 내에서 다른 함수를 정의할 수도 있습니다.

Fish는 `return` 값을 필요로 하지 않기 때문에 함수의 출력이 반환 값입니다. 그리고 미래의 세션에서도 지속적으로 접근할 수 있는 함수를 원한다면 `funcsave`를 기억하세요.

### 참고하기
- 함수에 대한 fish 튜토리얼: https://fishshell.com/docs/current/tutorial.html#tut_functions
- `function`에 대한 fish 문서: https://fishshell.com/docs/current/cmds/function.html
- fish에서 함수 작성에 관한 포괄적 가이드: https://fishshell.com/docs/current/index.html#syntax-function