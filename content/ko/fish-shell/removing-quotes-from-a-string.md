---
title:                "문자열에서 따옴표 제거하기"
date:                  2024-01-26T03:39:26.838050-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 따옴표 제거하기"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/removing-quotes-from-a-string.md"
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
