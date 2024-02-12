---
title:                "문자열 대문자화"
aliases:
- /ko/fish-shell/capitalizing-a-string/
date:                  2024-02-03T19:05:23.412968-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열을 대문자화한다는 것은 첫 글자를 대문자로, 나머지 문자열은 소문자로 수정하는 것을 의미합니다. 이는 텍스트 처리, 사용자 입력 정규화, 데이터 포맷팅 등에서 일관성을 확보하거나 특정 포맷팅 기준을 충족시키기 위해 흔히 수행되는 작업입니다.

## 어떻게 하나요:

Fish Shell에서는 외부 도구나 라이브러리 없이 내장 함수만을 이용해 직접 문자열을 조작할 수 있습니다. 문자열을 대문자화하기 위해서는 `string` 명령과 서브커맨드들을 결합할 수 있습니다.

```fish
# 샘플 문자열
set sample_string "hello world"

# 첫 글자 대문자화
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

출력:
```
Hello world
```

문자열 내 여러 단어를 대문자화해야 하는 시나리오(예: "hello world"를 "Hello World"로 변환)의 경우, 각 단어에 대해 대문자화 로직을 적용하면서 반복하게 됩니다:

```fish
# 샘플 문장
set sentence "hello fish shell programming"

# 각 단어 대문자화
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# 대문자화된 단어들을 합치기
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

출력:
```
Hello Fish Shell Programming
```

Fish Shell은 일부 프로그래밍 언어가 그들의 문자열 메서드를 사용하여 제공하는 것처럼 전체 문장 대문자화를 위한 단일 명령어 접근 방식을 직접 제공하지 않습니다. 따라서, `string split`, `string sub`, `string upper`을 결합한 후 다시 조합하는 것은 이를 달성하기 위한 Fish Shell에서의 관례적인 접근법을 나타냅니다.
