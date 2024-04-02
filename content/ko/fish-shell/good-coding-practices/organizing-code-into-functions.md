---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:54.005907-07:00
description: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574\
  \ \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uC77C\uBD80\uB97C \uBB36\uB294 \uAC83\uC5D0 \uB300\
  \uD574 \uC774\uC57C\uAE30\uD569\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC27D\uAC8C\
  \ \uC77D\uACE0, \uD14C\uC2A4\uD2B8\uD558\uBA70, \uC7AC\uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uAC8C \uD558\uAE30 \uC704\uD574\uC11C\uC785\uB2C8\uB2E4 \u2014 \uC544\uBB34\
  \uB3C4 \uCF54\uB4DC \uC2A4\uD30C\uAC8C\uD2F0\uC758 \uB2AA\uC744 \uD5E4\uCE58\uACE0\
  \ \uC2F6\uC5B4\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.864004-06:00'
model: gpt-4-0125-preview
summary: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uD2B9\uC815 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574 \uC2A4\
  \uD06C\uB9BD\uD2B8\uC758 \uC77C\uBD80\uB97C \uBB36\uB294 \uAC83\uC5D0 \uB300\uD574\
  \ \uC774\uC57C\uAE30\uD569\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC27D\uAC8C \uC77D\
  \uACE0, \uD14C\uC2A4\uD2B8\uD558\uBA70, \uC7AC\uC0AC\uC6A9\uD560 \uC218 \uC788\uAC8C\
  \ \uD558\uAE30 \uC704\uD574\uC11C\uC785\uB2C8\uB2E4 \u2014 \uC544\uBB34\uB3C4 \uCF54\
  \uB4DC \uC2A4\uD30C\uAC8C\uD2F0\uC758 \uB2AA\uC744 \uD5E4\uCE58\uACE0 \uC2F6\uC5B4\
  \uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uB294 \uBC29\uBC95"
weight: 18
---

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
