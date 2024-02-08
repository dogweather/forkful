---
title:                "명령줄 인수 읽기"
aliases:
- ko/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:21.881385-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
