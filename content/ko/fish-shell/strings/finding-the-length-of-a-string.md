---
date: 2024-01-20 17:47:33.484827-07:00
description: "How to (\uBC29\uBC95): Fish Shell\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\
  \uC758 \uAE38\uC774\uB97C \uC27D\uAC8C \uCC3E\uC744 \uC218 \uC788\uC5B4\uC694. `string\
  \ length` \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD574 \uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.839839-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C\
  \ \uC27D\uAC8C \uCC3E\uC744 \uC218 \uC788\uC5B4\uC694."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to (방법):
Fish Shell에서는 문자열의 길이를 쉽게 찾을 수 있어요. `string length` 명령어를 사용해 보세요:

```Fish Shell
set my_string "안녕하세요"
string length $my_string
```

출력 결과는 `5`입니다. 한글 문자도 잘 세고 있어요!

## Deep Dive (심층 분석):
과거에는 문자열의 길이를 찾기 위해 여러 복잡한 방법들이 사용되었지만, Fish Shell은 이 과정을 매우 단순화했습니다. 문자열의 길이를 세는 것은 언어 차원에서 지원되며, `string length`는 유니코드 문자도 올바르게 계산합니다. 다른 쉘 스크립트처럼 별도의 함수를 작성할 필요도 없습니다. Bash와 비교할 때, `wc -m` 같은 외부 도구에 의존하지 않고 내부 명령어로 처리하기 때문에 속도가 빠릅니다.

## See Also (관련 자료):
Fish Shell 공식 문서의 문자열 관련 섹션:
- [Fish Shell String Documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)

Stack Overflow, Fish Shell 커뮤니티가 주는 실제 사용 예제:
- [How to find the length of a string in shell?](https://stackoverflow.com/questions/17368067/length-of-string-in-shell)
