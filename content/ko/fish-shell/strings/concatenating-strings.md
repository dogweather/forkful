---
aliases:
- /ko/fish-shell/concatenating-strings/
date: 2024-01-20 17:35:09.370596-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC5EC\uB7EC \uBB38\uC790\uC5F4\
  \uC744 \uD558\uB098\uB85C \uD569\uCE58\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC774 \uB370\uC774\uD130\uB97C \uAD6C\uC131\uD558\uACE0\
  , \uCD9C\uB825\uC744 \uD615\uC2DD\uD654\uD558\uBA70, \uB3D9\uC801 \uAC12\uC744 \uC0DD\
  \uC131\uD560 \uB54C \uC790\uC8FC \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.865461
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC5EC\uB7EC \uBB38\uC790\uC5F4\uC744\
  \ \uD558\uB098\uB85C \uD569\uCE58\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC774 \uB370\uC774\uD130\uB97C \uAD6C\uC131\uD558\uACE0, \uCD9C\
  \uB825\uC744 \uD615\uC2DD\uD654\uD558\uBA70, \uB3D9\uC801 \uAC12\uC744 \uC0DD\uC131\
  \uD560 \uB54C \uC790\uC8FC \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결은 여러 문자열을 하나로 합치는 것입니다. 프로그래머들이 데이터를 구성하고, 출력을 형식화하며, 동적 값을 생성할 때 자주 사용합니다.

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
