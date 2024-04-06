---
date: 2024-01-20 17:45:31.303140-07:00
description: "How to: Fish Shell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB2E4\uB8E8\
  \uB294 \uAC83\uC740 \uC0C1\uB2F9\uD788 \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4. \uACFC\
  \uAC70\uC5D0\uB294 UNIX \uD658\uACBD\uC5D0\uC11C `cut`, `awk`, `sed` \uAC19\uC740\
  \ \uD234\uB4E4\uB85C \uC774 \uC791\uC5C5\uC744 \uD588\uC2B5\uB2C8\uB2E4. Fish\uB294\
  \ \uC774\uB7EC\uD55C \uC791\uC5C5\uC744 \uB354 \uAC04\uB2E8\uD558\uAC8C \uD560 \uC218\
  \ \uC788\uB294 `string` \uC790\uCCB4 \uBA85\uB839\uC5B4\uB97C \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. \uC608\uB97C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.428230-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uB2E4\uB8E8\uB294 \uAC83\
  \uC740 \uC0C1\uB2F9\uD788 \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4."
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
