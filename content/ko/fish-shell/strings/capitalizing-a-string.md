---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:23.412968-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Fish Shell\uC5D0\uC11C\uB294\
  \ \uC678\uBD80 \uB3C4\uAD6C\uB098 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC5C6\uC774 \uB0B4\
  \uC7A5 \uD568\uC218\uB9CC\uC744 \uC774\uC6A9\uD574 \uC9C1\uC811 \uBB38\uC790\uC5F4\
  \uC744 \uC870\uC791\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBB38\uC790\uC5F4\uC744\
  \ \uB300\uBB38\uC790\uD654\uD558\uAE30 \uC704\uD574\uC11C\uB294 `string` \uBA85\uB839\
  \uACFC \uC11C\uBE0C\uCEE4\uB9E8\uB4DC\uB4E4\uC744 \uACB0\uD569\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.828852-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC5D0\uC11C\uB294 \uC678\uBD80 \uB3C4\uAD6C\uB098 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC \uC5C6\uC774 \uB0B4\uC7A5 \uD568\uC218\uB9CC\uC744 \uC774\uC6A9\
  \uD574 \uC9C1\uC811 \uBB38\uC790\uC5F4\uC744 \uC870\uC791\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
