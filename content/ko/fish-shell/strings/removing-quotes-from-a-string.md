---
aliases:
- /ko/fish-shell/removing-quotes-from-a-string/
date: 2024-01-26 03:39:26.838050-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uADC0\uCC2E\uC740 \uB2E8\uC77C(' ') \uB610\uB294\
  \ \uC774\uC911(\" \") \uB530\uC634\uD45C\uB97C \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\
  \uC5D0\uC11C \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC785\uB825\uC744 \uC815\
  \uD654\uD558\uAC70\uB098 \uB530\uC634\uD45C\uAC00 \uC5C6\uB294 \uC0C1\uD0DC\uC5D0\
  \uC11C \uB370\uC774\uD130\uB97C \uCD94\uAC00 \uCC98\uB9AC\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.859841
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uADC0\uCC2E\uC740 \uB2E8\uC77C(' ') \uB610\uB294 \uC774\
  \uC911(\" \") \uB530\uC634\uD45C\uB97C \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uC5D0\
  \uC11C \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC785\uB825\uC744 \uC815\uD654\
  \uD558\uAC70\uB098 \uB530\uC634\uD45C\uAC00 \uC5C6\uB294 \uC0C1\uD0DC\uC5D0\uC11C\
  \ \uB370\uC774\uD130\uB97C \uCD94\uAC00 \uCC98\uB9AC\uD558\uAE30 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 따옴표를 제거한다는 것은 귀찮은 단일(' ') 또는 이중(" ") 따옴표를 텍스트 데이터에서 벗겨내는 것을 의미합니다. 프로그래머들은 종종 입력을 정화하거나 따옴표가 없는 상태에서 데이터를 추가 처리하기 위해 이 작업을 수행합니다.

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
