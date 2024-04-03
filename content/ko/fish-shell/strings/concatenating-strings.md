---
date: 2024-01-20 17:35:09.370596-07:00
description: "How to: (\uBC29\uBC95) Fish Shell\uC5D0\uC11C \uBB38\uC790\uC5F4 \uC5F0\
  \uACB0\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uBCC0\uC218\uB098 \uBB38\uC790\uC5F4\
  \uC744 \uBC14\uB85C \uC774\uC5B4 \uBD99\uC774\uC138\uC694. \uADF8\uB9AC\uACE0 \uACB0\
  \uACFC\uB97C \uD655\uC778\uD558\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.840981-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell\uC5D0\uC11C \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uAC04\uB2E8\
  \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (방법)
Fish Shell에서 문자열 연결은 간단합니다. 변수나 문자열을 바로 이어 붙이세요. 그리고 결과를 확인하세요.

```Fish Shell
# 문자열 연결하기
set greeting "안녕하세요, "
set name "철수님!"
echo $greeting$name
```

출력:
```
안녕하세요, 철수님!
```

```Fish Shell
# 문자열에 변수를 연결하여 새 변수 생성하기
set prefix "고객님의 번호는 "
set user_id "1234"
set message $prefix$user_id
echo $message
```

출력:
```
고객님의 번호는 1234
```

## Deep Dive (심층 분석)
Fish Shell에서 문자열 연결은 확장 방식(extensible)이며, 쉽고 직관적입니다. 과거의 다른 쉘과 달리, Fish는 추가적인 구문 없이도 직접적인 방식을 제공합니다. 예를 들어, Bash에서는 `echo "Hello, " . "World!"`와 같은 점 연산자가 필요했지만, Fish에서는 그냥 붙여 넣기만 하면 됩니다.

대안적으로, `string` 명령어를 사용하여 문자열을 조작할 수 있으며, 이는 더 복잡한 문자열 작업에 사용될 수 있습니다. 예를 들어:

```Fish Shell
string join '' "사과는" " 맛있어요!"
```

내부적으로, Fish는 문자열을 환경 변수처럼 다루며, 명령어 치환(command substitution)과 파이프라인(pipelining)을 통해 강력한 문자열 연산 기능을 제공합니다.

## See Also (참고 자료)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell GitHub 리포지터리](https://github.com/fish-shell/fish-shell)
