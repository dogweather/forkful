---
date: 2024-01-20 17:52:19.098857-07:00
description: "How to: (\uBC29\uBC95) BASH\uC5D0\uC11C \uB514\uBC84\uADF8 \uCD9C\uB825\
  \uC740 \uAC04\uB2E8\uD558\uAC8C \uC2DC\uC791\uD588\uC5B4\uC694. `echo`\uB098 `printf`\
  \ \uBA85\uB839\uC5B4\uB85C \uAE30\uBCF8\uC801\uC778 \uB514\uBC84\uADF8 \uC815\uBCF4\
  \uB97C \uCD9C\uB825\uD588\uC9C0\uC694. \uC2DC\uAC04\uC774 \uC9C0\uB098\uBA74\uC11C\
  , \uCF54\uB4DC\uAC00 \uBCF5\uC7A1\uD574\uC9C0\uBA74\uC11C \uC870\uAC74\uC5D0 \uB530\
  \uB77C \uB514\uBC84\uADF8 \uC815\uBCF4\uB97C \uBCF4\uC5EC\uC8FC\uAC70\uB098 \uC228\
  \uACA8\uC57C \uD560 \uD544\uC694\uAC00 \uC0DD\uACBC\uC5B4\uC694. \uD568\uC218\uB97C\
  \ \uC0AC\uC6A9\uD574 \uC870\uAC74\uBD80\uB85C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.157407-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) BASH\uC5D0\uC11C \uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uAC04\
  \uB2E8\uD558\uAC8C \uC2DC\uC791\uD588\uC5B4\uC694."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

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
