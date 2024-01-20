---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 뭐하고 왜하나요?
텍스트 파일을 읽는 것은 파일 내의 데이터를 액세스하는 방법입니다. 프로그래머들은 이것을 활용하여 정보를 수집하고, 분석하고, 원하는 작업을 수행하기 위해서 합니다.

## 어떻게 하나요?
Bash에서 텍스트 파일은 'cat', 'tail', 'head', 'awk' 등의 명령어 사용하여 읽을 수 있습니다.

```Bash
# 파일 전체 읽기
$ cat test.txt

# 파일의 마지막 5줄 읽기
$ tail -n 5 test.txt

# 파일의 첫 5줄 읽기
$ head -n 5 test.txt

# awk로 특정 필드 읽기
$ awk '{print $1}' test.txt
```
이런 방식으로 Bash는 텍스트 파일을 읽어 다양한 작업을 수행할 수 있습니다.

## 딥 다이브
텍스트 파일 읽기는 UNIX와 함께 시작되어 Bash에 그 기능이 상속되었습니다. 동일한 작업을 수행하는 여러 대안이 있습니다. 예를 들어, 'sed'와 'grep' 명령어를 사용하여 텍스트 데이터를 읽고 조작할 수 있습니다. Bash에서 텍스트 파일을 읽는 방법은 파일의 내용을 stdout에 출력하는 것을 기본으로 합니다. 이 정보를 파이프(|) 또는 리디렉션(>)를 사용하여 다른 명령어로 전달하거나 다른 파일로 출력할 수 있습니다.

## 참고 자료
- Bash Scripting Guide (https://tldp.org/LDP/abs/html/)
- Advanced Bash-Scripting Guide (https://tldp.org/LDP/abs/html/abs-guide.html)
- UNIX Text Processing (http://www.cs.cf.ac.uk/Dave/C/node3.html#SECTION00300000000000000000)

이들 자료는 Bash와 텍스트 파일 처리에 대한 깊은 이해를 돕습니다. 프로그래밍의 복잡성과 다양성을 충족시킨 Bash는 여전히 강력하고 필수적인 도구입니다.