---
date: 2024-01-26 03:39:26.838050-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694? Fish\uB294 \uC774\uB7F0 \uC885\
  \uB958\uC758 \uC791\uC5C5\uC744 \uC704\uD55C \uB0B4\uC7A5 \uB9C8\uBC95\uC744 \uAC00\
  \uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uB540\uC744 \uD758\uB9AC\uC9C0 \uC54A\uACE0\
  \ `string` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC138\uC694. \uC774 \uB9C8\uBC95\
  \uB4E4\uC744 \uD655\uC778\uD574 \uBCF4\uC138\uC694."
lastmod: '2024-03-13T22:44:55.835774-06:00'
model: gpt-4-0125-preview
summary: "Fish\uB294 \uC774\uB7F0 \uC885\uB958\uC758 \uC791\uC5C5\uC744 \uC704\uD55C\
  \ \uB0B4\uC7A5 \uB9C8\uBC95\uC744 \uAC00\uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 어떻게 하나요?
Fish는 이런 종류의 작업을 위한 내장 마법을 가지고 있습니다. 땀을 흘리지 않고 `string` 함수를 사용하세요. 이 마법들을 확인해 보세요:

```fish
# 단일 따옴표 예제
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # 출력: Hello, World!

# 이중 따옴표 예제도 똑같습니다
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # 출력: Hello, Universe!
```

## 심층 분석
명령줄 석기 시대에는 `sed`나 `awk`와 같은 도구를 이용해 따옴표를 제거하는 작업에 매달렸을 텐데요, 이는 복잡한 백슬래시와 암호 같은 플래그들의 진짜 혼란이었습니다. Fish의 `string` 함수는 더 새로운 시대에서 온 것으로, 코드를 더 깨끗하고 직관적으로 만듭니다.

다른 쉘의 대안은 여전히 이런 오래된 도구에 의존하거나 bash의 매개변수 확장이나 zsh의 수정자와 같은 자체 내장 메소드를 사용할 수 있습니다.

`string` 함수는 따옴표를 자르는 것 이상입니다. Fish에서 문자열 작업을 위한 스위스 아미 나이프와 같습니다. `string`을 사용하면 터미널 내에서 문자열을 자르고, 나누고, 합치고, 심지어 정규 표현식 일치까지 할 수 있습니다.

## 참고자료
공식 문서를 통해 `string`에 대해 더 깊이 파고들어 보세요:
- [Fish Shell 문자열 문서](https://fishshell.com/docs/current/commands.html#string)

전통적인 쉘로 스크립팅을 할 때나, 그리움을 느낄 때 다음을 확인해 보세요:
- [Sed & Awk 가이드](https://www.grymoire.com/Unix/Sed.html)
- [Bash 매개변수 확장](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
