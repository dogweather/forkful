---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
텍스트 파일을 쓴다는 것은 데이터를 텍스트 형식으로 저장하는 행위입니다. 프로그래머는 설정, 데이터 저장, 로깅 등 다양한 이유로 이를 수행합니다.

## How to:
```Bash
# 텍스트 파일 생성 및 작성
echo "Hello, World!" > hello.txt

# 파일 내용 확인
cat hello.txt
```
출력:
```
Hello, World!
```
```Bash
# 파일에 추가로 텍스트 작성
echo "안녕하세요, 리더님!" >> hello.txt

# 업데이트된 파일 내용 확인
cat hello.txt
```
출력:
```
Hello, World!
안녕하세요, 리더님!
```

## Deep Dive
초창기 유닉스 시스템부터, 리다이렉션(`>`, `>>`)과 같은 방식으로 텍스트 파일 작성이 가능했습니다. 대안으로 `vi`, `nano` 같은 텍스트 편집기 사용도 가능합니다만, 스크립트에서는 `echo`, `printf` 명령어가 흔히 쓰입니다. `>`는 파일을 새로 작성하거나 덮어씁니다. `>>`는 기존 파일에 내용을 추가합니다.

## See Also
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Echo Command in Linux: https://linuxize.com/post/echo-command-in-linux-with-examples/
