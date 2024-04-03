---
date: 2024-01-20 17:55:21.881385-07:00
description: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\
  \uC6A9\uC790\uAC00 \uD130\uBBF8\uB110\uC744 \uD1B5\uD574 \uC2A4\uD06C\uB9BD\uD2B8\
  \uC5D0 \uB370\uC774\uD130\uB97C \uC804\uB2EC\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC720\uC5F0\uC131\uC744\
  \ \uB192\uC774\uACE0 \uC0AC\uC6A9\uC790 \uB9DE\uCDA4\uD615 \uB3D9\uC791\uC744 \uAD6C\
  \uD604\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.504380-06:00'
model: gpt-4-1106-preview
summary: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\uC6A9\
  \uC790\uAC00 \uD130\uBBF8\uB110\uC744 \uD1B5\uD574 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\
  \ \uB370\uC774\uD130\uB97C \uC804\uB2EC\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\
  \uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## What & Why? (무엇이며 왜 사용하는가?)
커맨드 라인 인자 읽기는 사용자가 터미널을 통해 스크립트에 데이터를 전달할 수 있게 해줍니다. 프로그래머들은 유연성을 높이고 사용자 맞춤형 동작을 구현하기 위해 이를 사용합니다.

## How to (방법)
```Bash
#!/bin/bash
# script.sh

echo "첫 번째 인자: $1"
echo "두 번째 인자: $2"
echo "인자 전체: $@"
echo "인자 갯수: $#"

# 샘플 사용법: bash script.sh Apple Mango
```
```Bash
첫 번째 인자: Apple
두 번째 인자: Mango
인자 전체: Apple Mango
인자 갯수: 2
```

## Deep Dive (심층 탐구)
커맨드 라인 인자는 UNIX 시대부터 사용되어왔고, 스크립트간에 데이터를 넘기는 주된 방법입니다. `$1`, `$2`는 순서대로 인자를 가리킵니다. `$@`는 모든 인자를, `$#`는 인자의 개수를 나타냅니다.

대안으로는 `getopts` 또는 GNU의 `getopt`가 있어 복잡한 인자 구문 분석을 도울 수 있습니다. 이들은 옵션과 해당 값으로 이루어진 인자를 다룰 때 유용합니다.

내부적으로 스크립트는 위치 매개변수를 사용해 인자를 읽습니다. 스크립트 실행 시 쉘은 `$0`에 스크립트 이름을, `$1`부터 시작해서 각 인자를 할당합니다.

## See Also (관련 자료)
- Bash manual: https://www.gnu.org/software/bash/manual/
- Learn Shell Programming: https://linuxcommand.org/lc3_learning_the_shell.php
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
