---
date: 2024-01-20 17:47:10.729335-07:00
description: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB780 \uBB38\uC790\uC5F4\
  \uC5D0 \uD3EC\uD568\uB41C \uBB38\uC790 \uC218\uB97C \uC138\uB294 \uC791\uC5C5\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uAC80\uC99D\
  , \uBB38\uC790 \uCC98\uB9AC, \uD639\uC740 \uAE30\uD0C0 \uC81C\uC57D \uC870\uAC74\
  \uC744 \uCDA9\uC871\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.466104-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB780 \uBB38\uC790\uC5F4\uC5D0\
  \ \uD3EC\uD568\uB41C \uBB38\uC790 \uC218\uB97C \uC138\uB294 \uC791\uC5C5\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uAC80\uC99D, \uBB38\
  \uC790 \uCC98\uB9AC, \uD639\uC740 \uAE30\uD0C0 \uC81C\uC57D \uC870\uAC74\uC744 \uCDA9\
  \uC871\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## What & Why? (무엇 & 왜?)
문자열 길이 찾기란 문자열에 포함된 문자 수를 세는 작업입니다. 프로그래머는 데이터 검증, 문자 처리, 혹은 기타 제약 조건을 충족하기 위해 이를 수행합니다.

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
