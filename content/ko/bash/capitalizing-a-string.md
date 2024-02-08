---
title:                "문자열 대문자화"
aliases:
- ko/bash/capitalizing-a-string.md
date:                  2024-02-03T19:04:56.157338-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Bash에서 문자열의 첫 글자를 대문자로 변환하면서 나머지 문자열은 변경하지 않는 것을 의미합니다. 이 기술은 출력 포맷을 지정하거나, 일부 문자열이 가독성이나 스타일 취향을 위해 대문자로 시작해야 하는 코딩 관례를 준수하는 데 일반적으로 사용됩니다.

## 어떻게 하나요:

Bash에는 문자열을 대문자화하는 구체적인 내장 함수가 없지만, 파라미터 확장이나 `awk` 같은 외부 도구를 사용하여 이 작업을 수행할 수 있습니다. 다음은 Bash에서 문자열을 대문자화하는 몇 가지 방법입니다:

**파라미터 확장 사용하기:**

이 방법은 셸에서 직접 문자열을 조작합니다.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
출력:
```
Hello world
```

**`awk` 사용하기:**

`awk`는 대부분의 유닉스 계열 운영 체제에서 사용할 수 있는 강력한 텍스트 처리 도구로, 문자열을 대문자화하는 데 활용할 수 있습니다.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
출력:
```
Hello world
```

**`sed` 사용하기:**

전통적인 접근 방식으로, `sed`를 사용하여 문자열의 첫 글자를 대문자로 만들 수 있습니다. 하지만 이전 방법들에 비해 약간 더 복잡합니다.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
출력:
```
Hello world
```

이 스니펫들은 Bash에서 문자열의 첫 글자를 대문자로 만드는 방법을 보여주며, 텍스트를 조작할 때 셸 스크립팅의 유연성을 강조합니다.
