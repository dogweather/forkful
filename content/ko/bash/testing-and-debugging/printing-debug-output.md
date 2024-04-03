---
date: 2024-01-20 17:52:19.098857-07:00
description: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uAC00 \uC5B4\uB5BB\
  \uAC8C \uC2E4\uD589\uB418\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uBA54\uC2DC\uC9C0\
  \uB97C \uD45C\uC2DC\uD558\uB294 \uAC83\uC774\uC5D0\uC694. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uCC3E\uACE0 \uD504\uB85C\uADF8\uB7A8\uC758\
  \ \uB3D9\uC791\uC744 \uC774\uD574\uD558\uAE30 \uC704\uD574 \uC774\uAC78 \uC0AC\uC6A9\
  \uD574\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.485427-06:00'
model: gpt-4-1106-preview
summary: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uCF54\uB4DC\uAC00 \uC5B4\uB5BB\uAC8C\
  \ \uC2E4\uD589\uB418\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uBA54\uC2DC\uC9C0\uB97C\
  \ \uD45C\uC2DC\uD558\uB294 \uAC83\uC774\uC5D0\uC694."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## What & Why? (무엇인가요? 왜 사용하나요?)
디버그 출력은 코드가 어떻게 실행되는지 확인하는 메시지를 표시하는 것이에요. 프로그래머들은 버그를 찾고 프로그램의 동작을 이해하기 위해 이걸 사용해요.

## How to: (방법)
```Bash
# 기본적인 메시지 출력
echo "디버그 메시지: 변수값은 $VARIABLE 입니다."

# 조건에 따른 메시지 출력
if [ "$VARIABLE" -eq 1 ]; then
  echo "디버그: VARIABLE은 1입니다."
else
  echo "디버그: VARIABLE은 1이 아닙니다."
fi

# 디버그 모드가 활성화되었을 때 메시지 출력
DEBUG_MODE=1
debug() {
  if [ "$DEBUG_MODE" -eq 1 ]; then
    echo "디버그: $1"
  fi
}

# 디버그 함수 사용 예
debug "이것은 디버그 모드에서만 보입니다."
```
출력:
```
디버그 메시지: 변수값은 입니다.
디버그: VARIABLE은 1이 아닙니다.
디버그: 이것은 디버그 모드에서만 보입니다.
```

## Deep Dive (깊이 알아보기)
BASH에서 디버그 출력은 간단하게 시작했어요. `echo`나 `printf` 명령어로 기본적인 디버그 정보를 출력했지요. 시간이 지나면서, 코드가 복잡해지면서 조건에 따라 디버그 정보를 보여주거나 숨겨야 할 필요가 생겼어요. 함수를 사용해 조건부로 메시지를 출력하거나, 디버그 레벨을 정해 출력의 정도를 조절하는 기법들이 생겨났죠. `set -x`를 사용하면 스크립트가 실행되는 각 줄의 실행 전 명령어를 보여줄 수 있어요. 이 방법은 다른 방식들보다 더 자세한 실행 정보를 제공할 수 있죠. 그리고, 터미널에 출력하는 대신에 `2>`를 사용해 오류 메시지만 파일에 redirect할 수도 있어요.

## See Also (더 보기)
- Bash debugging techniques: [https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#The-Set-Builtin](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#The-Set-Builtin)
- Advanced Bash scripting guide: [https://www.tldp.org/LDP/abs/html/debugging.html](https://www.tldp.org/LDP/abs/html/debugging.html)
- Redirecting output in Bash: [https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html)
