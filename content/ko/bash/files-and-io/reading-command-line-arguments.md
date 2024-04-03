---
date: 2024-01-20 17:55:21.881385-07:00
description: "How to (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.504380-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

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
