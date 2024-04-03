---
date: 2024-01-20 17:45:31.303140-07:00
description: "\uBB34\uC5C7\uC774\uBA70 \uC65C \uC0AC\uC6A9\uD558\uB294\uAC00? \uBB38\
  \uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uBD80\uBD84\uC744 \uCD94\uCD9C\uD558\uB294\
  \ \uAC83\uC744 'substring \uCD94\uCD9C'\uC774\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uC774\
  \uB7F0 \uC791\uC5C5\uC740 \uB370\uC774\uD130 \uD30C\uC2F1, \uC0AC\uC6A9\uC790 \uC785\
  \uB825 \uCC98\uB9AC, \uD639\uC740 \uD14D\uC2A4\uD2B8 \uAE30\uBC18\uC758 \uC870\uAC74\
  \ \uD655\uC778 \uC2DC \uD544\uC694\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.837057-06:00'
model: gpt-4-1106-preview
summary: "\uBB34\uC5C7\uC774\uBA70 \uC65C \uC0AC\uC6A9\uD558\uB294\uAC00."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to:
```Fish Shell
# 문자열 정의
set full_string "Fish Shell is fantastic!"

# 인덱스 6부터 10개 문자 추출
set substring (string sub -s 6 -l 10 -- $full_string)
echo $substring  # 출력: Shell is f

# 인덱스 6부터 끝까지 문자 추출
set substring_end (string sub -s 6 -- $full_string)
echo $substring_end  # 출력: Shell is fantastic!
```

## Deep Dive
Fish Shell에서 문자열을 다루는 것은 상당히 직관적입니다. 과거에는 UNIX 환경에서 `cut`, `awk`, `sed` 같은 툴들로 이 작업을 했습니다. Fish는 이러한 작업을 더 간단하게 할 수 있는 `string` 자체 명령어를 제공합니다. 예를 들어, `string sub` 명령은 문자열을 쉽게 추출할 수 있게 해줍니다. 인덱스와 길이, 혹은 시작점과 끝점을 정해서 원하는 부분만 가져올 수 있죠. 별도의 복잡한 정규 표현식이나 외부 프로그램 없이도 말이죠.

## See Also
- [Fish Shell Documentation on String Manipulation](https://fishshell.com/docs/current/cmds/string.html)
- [Stack Overflow: How to extract a substring in Fish shell](https://stackoverflow.com/questions/tagged/fish)
