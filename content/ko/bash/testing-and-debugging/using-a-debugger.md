---
date: 2024-01-26 03:47:42.952480-07:00
description: "\uC5B4\uB5BB\uAC8C: Bash\uB294 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uCC98\
  \uB7FC \uB0B4\uC7A5\uD615 \uB514\uBC84\uAC70\uAC00 \uC5C6\uC9C0\uB9CC, `set -x`\uC640\
  \ \uAC19\uC740 \uB0B4\uC7A5 \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB34\
  \uC2A8 \uC77C\uC774 \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\uC9C0 \uCD94\uC801\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB610\uB294 \uC5C5\uADF8\uB808\uC774\uB4DC\uB97C\
  \ \uC704\uD574\uC11C, \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC9C4\uD589\uD560\
  \ \uC218 \uC788\uB294 \uC801\uC808\uD55C \uB514\uBC84\uAC70\uC778 `bashdb`\uAC00\
  \ \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30\uC5D0 \uC608\uC2DC\uAC00\u2026"
lastmod: '2024-03-13T22:44:55.488390-06:00'
model: gpt-4-0125-preview
summary: "Bash\uB294 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uB0B4\uC7A5\uD615\
  \ \uB514\uBC84\uAC70\uAC00 \uC5C6\uC9C0\uB9CC, `set -x`\uC640 \uAC19\uC740 \uB0B4\
  \uC7A5 \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB34\uC2A8 \uC77C\uC774\
  \ \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\uC9C0 \uCD94\uC801\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 어떻게:
Bash는 다른 언어들처럼 내장형 디버거가 없지만, `set -x`와 같은 내장 명령어를 사용하여 무슨 일이 일어나고 있는지 추적할 수 있습니다. 또는 업그레이드를 위해서, 코드를 단계별로 진행할 수 있는 적절한 디버거인 `bashdb`가 있습니다. 여기에 예시가 있습니다:

```Bash
# set -x를 사용하여 디버깅하기
set -x
echo "디버깅 시작"
my_var="안녕, 디버깅 세계!"
echo $my_var
set +x

# bashdb 사용하기
# 패키지 관리자를 사용하여 bashdb 설치하기, 예: apt, yum, brew.
# my_script.sh라는 스크립트 디버깅하기:
bashdb my_script.sh
```

`set -x`를 사용하여 실행할 때의 출력:
```Bash
+ echo '디버깅 시작'
디버깅 시작
+ my_var='안녕, 디버깅 세계!'
+ echo '안녕, 디버깅 세계!'
안녕, 디버깅 세계!
+ set +x
```

## 심층 분석
역사적으로, Bash 스크립트를 디버깅하면 코드에 `echo` 문을 남발하는 것을 의미했습니다. 그러나 그 다음에 `set -x`가 등장하여 수동 출력 없이 런타임 실행을 엿볼 수 있게 되었습니다. 그리고 더 많은 제어를 원하는 사람들을 위해 C/C++용 gdb 디버거에서 영감을 받은 `bashdb` 디버거가 등장했습니다.

대안에 관해서는, `set` 명령어(`-x`, `-v`, `-e`)를 넘어, 다른 옵션으로는 분석을 위해 출력을 파일로 리다이렉션하거나 ShellCheck과 같은 외부 도구를 사용하여 정적 분석을 하는 것이 포함됩니다.

구현 측면에서, `set -x`는 쉽습니다; 실행되는 명령과 그 인자들을 출력하는 네이티브 Bash 옵션입니다. 반면에, `bashdb`는 코드를 단계별로 진행하고, 중단점을 설정하고, 표현식을 평가하는 것과 같은 기능을 허용합니다 - 이는 좀 더 까다로운 버그에 맞서 싸울 수 있는 기회를 제공합니다.

## 참고 자료
- Bash 디버거 프로젝트: http://bashdb.sourceforge.net/
- Chris Johnson과 Jayant Varma의 "Pro Bash Programming"을 통해 고급 스크립팅 학습.
- 정적 분석을 위한 ShellCheck: https://www.shellcheck.net/
