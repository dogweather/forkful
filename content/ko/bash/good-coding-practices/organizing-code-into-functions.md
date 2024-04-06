---
date: 2024-01-26 01:09:23.647862-07:00
description: "\uBC29\uBC95: Bash\uC5D0\uC11C \uD568\uC218\uB294 \uCD08\uAE30 \uBC84\
  \uC804\uBD80\uD130 \uCF54\uB4DC\uB97C \uAD6C\uD68D\uD558\uB294 \uBC29\uBC95\uC774\
  \uC5C8\uC2B5\uB2C8\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C, \uD568\uC218 \uC0AC\uC6A9\
  \uC740 \uCF54\uB4DC \uD488\uC9C8\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 1960\uB144\
  \uB300\uC5D0 \uB3C4\uC785\uB41C \uAD6C\uC870\uC801 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC6D0\uCE59\uACFC \uC77C\uCE58\uD569\uB2C8\uB2E4. \uD568\uC218\uC758 \uB300\uC548\
  \uC5D0\uB294 \uC2A4\uD06C\uB9BD\uD2B8 \uD30C\uC77C\uC758 \uC18C\uC2F1\uC774\uB098\
  \ \uBCC4\uCE6D(alias) \uC0AC\uC6A9\uC774 \uD3EC\uD568\uB418\uB098, \uC774\uB4E4\uC740\
  \ \uD568\uC218\uAC00\u2026"
lastmod: '2024-04-05T22:51:09.777044-06:00'
model: gpt-4-1106-preview
summary: "Bash\uC5D0\uC11C \uD568\uC218\uB294 \uCD08\uAE30 \uBC84\uC804\uBD80\uD130\
  \ \uCF54\uB4DC\uB97C \uAD6C\uD68D\uD558\uB294 \uBC29\uBC95\uC774\uC5C8\uC2B5\uB2C8\
  \uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

## 방법:
Bash에서 간단한 함수를 생성합니다:

```Bash
greet() {
  echo "안녕하세요, $1님!"
}
```

매개변수를 가지고 함수를 호출하여 사용합니다:

```Bash
greet "세계"  # 출력: 안녕하세요, 세계님!
```

함수는 실제 데이터 반환을 위해서가 아닌, 수치상의 상태 코드를 반환하기 위해 `return`을 사용할 수 있습니다:

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # 출력: 7
```

`$?`는 마지막 명령의 반환 값인 `add`의 수치 결과를 캡처한다는 점에 주목하세요.

## 심층 분석
Bash에서 함수는 초기 버전부터 코드를 구획하는 방법이었습니다. 역사적으로, 함수 사용은 코드 품질을 개선하기 위해 1960년대에 도입된 구조적 프로그래밍 원칙과 일치합니다.

함수의 대안에는 스크립트 파일의 소싱이나 별칭(alias) 사용이 포함되나, 이들은 함수가 제공하는 모듈성과 재사용성 수준을 제공하지 않습니다.

Bash에서 주목할 만한 구현 세부 사항은 함수가 일급 시민이라는 점입니다. 다른 언어에서 `function`과 같은 특정 선언 키워드가 없으나, Bash에서는 가독성을 위해 `function`이 선택적입니다. 함수의 스코프 또한 흥미롭습니다 - 변수는 기본적으로 전역적(global)이며, 제대로 관리되지 않으면 예상치 못한 동작을 초래할 수 있는 local로 선언되지 않은 한입니다.

## 참고 자료
- 쉘 함수에 대한 Bash 매뉴얼: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- 고급 Bash-스크립팅 가이드: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" 함수 스크립팅 개념 및 실무에 대한 심층적인 내용.
