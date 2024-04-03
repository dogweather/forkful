---
date: 2024-01-20 17:47:10.729335-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.466104-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게:)
```Bash
# 문자열 길이 찾기

# 변수에 문자열 할당
my_string="안녕하세요!"

# 문자열 길이 구하기
length=${#my_string}

# 길이 출력
echo $length
```

예상 출력:

```
9
```

## Deep Dive (심층 분석)
Bash에서 문자열 길이를 찾는 방법은 간단합니다. `${#변수명}` 구문을 쓰면 됩니다. 이 구문은 3버전부터 사용되었으므로 오래된 스크립트에서도 잘 동작합니다.

대안으로 `expr length "$my_string"` 명령을 사용할 수 있지만, POSIX 표준이 아니며 더 느립니다. 

한 가지 주의해야 할 점은 다국어 지원입니다. 위 예시처럼 한글을 다룰 때는 길이가 예상과 다를 수 있습니다. Bash는 기본적으로 바이트 단위로 길이를 계산합니다. 유니코드 문자열을 올바르게 처리하려면 `wc` 명령어와 `LANG` 환경 변수를 조정하거나 다른 도구를 사용해야 할 수 있습니다.

## See Also (참고 자료)
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash 문자열 처리에 대한 공식 문서: http://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Stack Overflow Bash FAQ: https://stackoverflow.com/questions/tagged/bash
