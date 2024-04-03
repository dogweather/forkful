---
date: 2024-01-20 17:41:59.995400-07:00
description: "\uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\
  \uB294 \uAC83\uC740 \uD2B9\uC815 \uC870\uAC74\uC744 \uCDA9\uC871\uD558\uB294 \uBB38\
  \uC790\uB4E4\uC744 \uBB38\uC790\uC5F4\uC5D0\uC11C \uC81C\uAC70\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\
  \uD130\uB97C \uC815\uC81C\uD558\uAC70\uB098, \uD544\uC694\uC5C6\uB294 \uBD80\uBD84\
  \uC744 \uC81C\uAC70\uD558\uC5EC \uC790\uB8CC\uB97C \uAC00\uACF5\uD558\uB294 \uB370\
  \ \uC774 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.830310-06:00'
model: gpt-4-1106-preview
summary: "\uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\uB294\
  \ \uAC83\uC740 \uD2B9\uC815 \uC870\uAC74\uC744 \uCDA9\uC871\uD558\uB294 \uBB38\uC790\
  \uB4E4\uC744 \uBB38\uC790\uC5F4\uC5D0\uC11C \uC81C\uAC70\uD558\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## What & Why? (무엇과 왜?)
패턴에 맞는 문자를 삭제하는 것은 특정 조건을 충족하는 문자들을 문자열에서 제거하는 과정입니다. 프로그래머들은 데이터를 정제하거나, 필요없는 부분을 제거하여 자료를 가공하는 데 이 방법을 사용합니다.

## How to: (방법)
Fish Shell에서 문자 패턴에 맞는 문자를 삭제하려면 `string` 명령어를 활용합니다. 여기에 몇 가지 예시를 보여드리겠습니다:

기본 패턴 삭제:
```Fish Shell
echo "fish_shell_rocks" | string replace -r "_.*" ""
# 출력: fish
```

특정 문자 삭제:
```Fish Shell
echo "fish123" | string replace -a "1" ""
# 출력: fish23
```

여러 문자 삭제:
```Fish Shell
echo "f.i.s.h" | string replace -a "." ""
# 출력: fish
```

## Deep Dive (심층 분석)
Fish Shell의 `string` 명령은 Fish 버전 2.3.0에서 도입되었으며, 문자열 조작 기능을 단순화하기 위해 추가되었습니다. `string replace`는 특정 문자열 또는 패턴을 찾아 다른 문자열로 치환하거나 삭제할 수 있습니다. `grep`, `awk`, `sed`와 같은 전통적인 유닉스 명령어들도 문자 삭제에 사용할 수 있지만, Fish는 `string`을 통해 이러한 기능을 내장 명령어로 제공함으로써 사용자의 편의성을 향상시켰습니다. `string replace`는 정규표현식을 지원하고 `-a` (모두 대체) 옵션을 사용하여 여러 개의 문자를 한 번에 삭제할 수 있습니다.

## See Also (관련 자료)
- Fish Shell의 공식 문서 (string): https://fishshell.com/docs/current/cmds/string.html
- 정규 표현식에 대한 정리: https://www.regular-expressions.info/
- Unix 명령어 `sed` 튜토리얼: https://www.gnu.org/software/sed/manual/sed.html
