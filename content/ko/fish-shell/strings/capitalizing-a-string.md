---
aliases:
- /ko/fish-shell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:23.412968-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C, \uB098\uBA38\uC9C0\
  \ \uBB38\uC790\uC5F4\uC740 \uC18C\uBB38\uC790\uB85C \uC218\uC815\uD558\uB294 \uAC83\
  \uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB294 \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  , \uC0AC\uC6A9\uC790 \uC785\uB825 \uC815\uADDC\uD654, \uB370\uC774\uD130 \uD3EC\uB9F7\
  \uD305 \uB4F1\uC5D0\uC11C \uC77C\uAD00\uC131\uC744 \uD655\uBCF4\uD558\uAC70\uB098\
  \ \uD2B9\uC815 \uD3EC\uB9F7\uD305 \uAE30\uC900\uC744 \uCDA9\uC871\uC2DC\uD0A4\uAE30\
  \ \uC704\uD574 \uD754\uD788 \uC218\uD589\uB418\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  ."
lastmod: 2024-02-18 23:09:06.852667
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C, \uB098\uBA38\uC9C0 \uBB38\
  \uC790\uC5F4\uC740 \uC18C\uBB38\uC790\uB85C \uC218\uC815\uD558\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB294 \uD14D\uC2A4\uD2B8 \uCC98\uB9AC,\
  \ \uC0AC\uC6A9\uC790 \uC785\uB825 \uC815\uADDC\uD654, \uB370\uC774\uD130 \uD3EC\uB9F7\
  \uD305 \uB4F1\uC5D0\uC11C \uC77C\uAD00\uC131\uC744 \uD655\uBCF4\uD558\uAC70\uB098\
  \ \uD2B9\uC815 \uD3EC\uB9F7\uD305 \uAE30\uC900\uC744 \uCDA9\uC871\uC2DC\uD0A4\uAE30\
  \ \uC704\uD574 \uD754\uD788 \uC218\uD589\uB418\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
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
