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

## 왜

이 글을 읽는 이유는 무엇일까요? 텍스트 파일을 읽는 것은 컴퓨팅에서 매우 중요한 활동 중 하나입니다. 예를 들어, 실행 파일 및 스크립트 파일과 같은 다른 유형의 파일을 읽을 수 있기 때문입니다.

## 어떻게

파일을 읽는 방법은 어떻게 될까요? 다음 코드 블록을 참고해주세요.

```Bash
#!/bin/bash
# 샘플 텍스트 파일을 읽어와 변수에 저장합니다.
text=$(cat sample.txt)
# 변수에 저장된 텍스트 출력하기
echo $text
```

출력 결과는 다음과 같습니다.

```
Hello, world! This is a sample text file.
```

## 더 깊게 들어가보기

이제 텍스트 파일을 읽는 방법에 대해 알게 되었습니다. 그렇다면 실제로 파일을 읽는 과정은 어떻게 이루어질까요? 이 과정에서 발생할 수 있는 문제는 무엇일까요? 이러한 깊이 있는 정보를 다음 링크에서 더 알아볼 수 있습니다.

## 참고

- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Reading and Writing Files in Bash](https://linuxize.com/post/bash-read-write-file/)