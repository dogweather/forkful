---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:56.157338-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Bash\uC5D0\uB294 \uBB38\uC790\
  \uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAD6C\uCCB4\uC801\uC778 \uB0B4\
  \uC7A5 \uD568\uC218\uAC00 \uC5C6\uC9C0\uB9CC, \uD30C\uB77C\uBBF8\uD130 \uD655\uC7A5\
  \uC774\uB098 `awk` \uAC19\uC740 \uC678\uBD80 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uB2E4\uC74C\uC740 Bash\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\
  \uD654\uD558\uB294 \uBA87 \uAC00\uC9C0 \uBC29\uBC95\uC785\uB2C8\uB2E4: **\uD30C\uB77C\
  \uBBF8\uD130 \uD655\uC7A5 \uC0AC\uC6A9\uD558\uAE30:** \uC774 \uBC29\uBC95\uC740\u2026"
lastmod: '2024-03-13T22:44:55.454816-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uB294 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\
  \uB294 \uAD6C\uCCB4\uC801\uC778 \uB0B4\uC7A5 \uD568\uC218\uAC00 \uC5C6\uC9C0\uB9CC\
  , \uD30C\uB77C\uBBF8\uD130 \uD655\uC7A5\uC774\uB098 `awk` \uAC19\uC740 \uC678\uBD80\
  \ \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774 \uC791\uC5C5\uC744 \uC218\uD589\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
