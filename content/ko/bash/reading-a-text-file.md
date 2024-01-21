---
title:                "텍스트 파일 읽기"
date:                  2024-01-20T17:53:43.703029-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

파일 읽기는 텍스트 파일의 내용을 가져와서 사용하는 것입니다. 프로그래머들은 데이터를 처리하거나 설정 정보를 불러올 때 이 방법을 사용합니다.

## How to: (방법)

```Bash
# 파일 전체를 한 번에 읽기
cat my_file.txt

# 한 줄씩 읽어서 처리하기
while IFS= read -r line; do
  echo "Line: $line"
done < my_file.txt

# 'head'를 사용하여 파일의 처음 10줄 읽기
head my_file.txt

# 'tail'를 사용하여 파일의 마지막 10줄 읽기
tail my_file.txt
```

## Deep Dive (심층 분석)

텍스트 파일을 읽는 것은 UNIX의 초기 시절부터 있어왔습니다. `cat` 명령어는 'concatenate'의 줄임말로, 파일의 내용을 쉽게 출력할 수 있게 해줍니다. `head`와 `tail`은 각각 파일의 시작부와 끝부의 내용을 빠르게 볼 수 있게 해줍니다. 위에 나온 while 루프 방식으로 파일을 줄 단위로 읽으면, 대용량 파일을 다룰 때 효율적입니다. 대안으로 스크립트 언어인 `awk`, `sed`, `perl` 등이 있으며, 이들은 복잡한 텍스트 처리에 사용됩니다.

## See Also (더 보기)

- GNU Coreutils: https://www.gnu.org/software/coreutils/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash Reference Manual: https://www.gnu.org/software/bash/manual/